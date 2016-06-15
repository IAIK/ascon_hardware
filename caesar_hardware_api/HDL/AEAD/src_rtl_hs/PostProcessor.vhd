-------------------------------------------------------------------------------
--! @file       PostProcessor.vhd
--! @brief      Post-processing unit for an authenticated encryption module.
--! @project    CAESAR Candidate Evaluation
--! @author     Ekawat (ice) Homsirikamol
--! @copyright  Copyright (c) 2016 Cryptographic Engineering Research Group
--!             ECE Department, George Mason University Fairfax, VA, U.S.A.
--!             All rights Reserved.
--! @license    This project is released under the GNU Public License.
--!             The license and distribution terms for this file may be
--!             found in the file LICENSE in this distribution or at
--!             http://www.gnu.org/licenses/gpl-3.0.txt
--! @note       This is publicly available encryption source code that falls
--!             under the License Exception TSU (Technology and software-
--!             —unrestricted)
--! PISO used within this unit follows the following convention:
--! > Order at the PISO input (left to right)      :  A(0) A(1) A(2) … A(N-1)
--! > Order at the PISO output (time 0 to time N-1):  A(0) A(1) A(2) … A(N-1)
--! > Order in the test vector file (left to right):  A(0) A(1) A(2) … A(N-1)
--! where A is a single I/O word.
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.AEAD_pkg.all;

entity PostProcessor is
    generic (
        --! I/O size (bits)
        G_W                 : integer := 32;    --! Public data input
        G_SW                : integer := 32;    --! Secret data input
        --! Reset behavior
        G_ASYNC_RSTN        : boolean := False; --! Async active low reset
        --! Special features activation
        G_CIPH_EXP          : boolean := False; --! Ciphertext expansion
        G_REVERSE_CIPH      : boolean := False; --! Reversed ciphertext
        G_MERGE_TAG         : boolean := False; --! Merge tag with data segment
        --! Block size (bits)
        G_DBLK_SIZE         : integer := 128;   --! Data
        G_TAG_SIZE          : integer := 128;   --! Key
        --! The number of bits required to hold block size expressed in
        --! bytes = log2_ceil(G_DBLK_SIZE/8)
        G_LBS_BYTES         : integer := 4
    );
    port (
        --! Global ports
        clk                 : in  std_logic;
        rst                 : in  std_logic;
        --! Data out ports
        do_data             : out std_logic_vector(G_W          -1 downto 0);
        do_ready            : in  std_logic;
        do_valid            : out std_logic;
        --! Header ports
        cmd                 : in  std_logic_vector(24           -1 downto 0);
        cmd_valid           : in  std_logic;
        cmd_ready           : out std_logic;
        --! CipherCore
        bdo                 : in  std_logic_vector(G_DBLK_SIZE  -1 downto 0);
        bdo_valid           : in  std_logic;
        bdo_ready           : out std_logic;
        bdo_size            : in  std_logic_vector(G_LBS_BYTES+1-1 downto 0);
        msg_auth_done       : in  std_logic;
        msg_auth_valid      : in  std_logic
    );
end PostProcessor;

architecture structure of PostProcessor is
    constant IS_BUFFER      : boolean := not (G_W = G_DBLK_SIZE);
    constant WB             : integer := G_W/8; --! Word bytes
    constant LOG2_WB        : integer := log2_ceil(WB);
    constant CNT_DWORDS     : integer := (G_DBLK_SIZE+(G_W-1))/G_W;
    constant ZEROS          : std_logic_vector(G_W      -1 downto 0)
        := (others => '0');
    --! =======================================================================
    type t_lookup is array (0 to (WB-1))
        of std_logic_vector(WB-1 downto 0);
    function getVbytesLookup(size: integer) return t_lookup is
        variable ret : t_lookup;
    begin
        for i in 0 to ((size/8)-1) loop
            if (i = 0) then
                ret(i) := (others => '0');
            else
                ret(i)(size/8-1   downto size/8-i) := (others => '1');
                ret(i)(size/8-i-1 downto 0)        := (others => '0');
            end if;
        end loop;
        return ret;
    end function getVbytesLookup;
    constant VBYTES_LOOKUP  : t_lookup := getVbytesLookup(G_W);
    --! =======================================================================
    --! Control
    signal en_len           : std_logic;
    signal en_s             : std_logic;
    signal en_ctr           : std_logic;
    signal ld_tag           : std_logic;
    signal ld_stat          : std_logic;
    signal ld_ctr           : std_logic;
    signal ld_sgmt_info     : std_logic;
    signal ld_ciph_exp_len  : std_logic;
    signal sel_last         : std_logic;
    signal sel_do           : std_logic;
    signal set_first        : std_logic;
    signal clr_first        : std_logic;
    --! Status
    signal is_decrypt       : std_logic;
    signal is_first         : std_logic;        --! is first block status
    signal is_ciph_exp_len  : std_logic;        --! is ciph_exp len status
    signal cpl_tag          : std_logic;        --! Completed tag header
    signal cpl_stat         : std_logic;        --! Completed status header
    signal sgmt_type        : std_logic_vector(4                -1 downto 0);
    signal sgmt_len         : std_logic_vector(16               -1 downto 0);
    signal sgmt_partial     : std_logic;
    signal sgmt_eot         : std_logic;
    signal sgmt_eoi         : std_logic;
    --! =======================================================================
    --! Datapath'
    --!     Signals
    signal word_size        : std_logic_vector(LOG2_WB          -1 downto 0);
    signal vbytes           : std_logic_vector(WB               -1 downto 0);
    signal out_word         : std_logic_vector(G_W              -1 downto 0);
    signal out_hdr          : std_logic_vector(G_W              -1 downto 0);
    signal is_eoi           : std_logic;
    --!     Registers
    signal msg_auth_done_r  : std_logic;
    signal msg_auth_valid_r : std_logic;
    signal reg_bdo          : std_logic_vector(G_DBLK_SIZE      -1 downto 0);
    signal reg_bdo_ready    : std_logic;
    signal reg_vbits        : std_logic_vector(G_W              -1 downto 0);
    signal reg_vbytes       : std_logic_vector(WB               -1 downto 0);
    signal ctr              : std_logic_vector
        (log2_ceil(CNT_DWORDS)-1 downto 0);
    --! =======================================================================
    --! aliases
    --!     Signal
    signal cmd_instr_opcode : std_logic_vector(4                -1 downto 0);
    signal cmd_sgmt_type    : std_logic_vector(4                -1 downto 0);
    signal cmd_sgmt_partial : std_logic;
    signal cmd_sgmt_eot     : std_logic;
    signal cmd_sgmt_eoi     : std_logic;
    signal cmd_sgmt_len     : std_logic_vector(16               -1 downto 0);
    --!     Global
    signal cmd_rdy          : std_logic;
    signal do_vld           : std_logic;
    signal bdo_rdy          : std_logic;
    --! =======================================================================
    type t_state is (S_WAIT_INSTR, S_WAIT_HDR, S_PREP,
        S_OUT, S_GEN_TAG_HDR, S_GEN_STAT_HDR, S_WAIT_BDO, S_WAIT_BDO_CIPH);
    signal cs   : t_state;  --! Current state
    signal ns   : t_state;  --! Next state
begin
    --! =======================================================================
    --! Datapath registers and control status registers
    --! =======================================================================
    process(clk)
    begin
        if rising_edge(clk) then
            if (cs = S_WAIT_INSTR) then
                is_decrypt <= cmd_instr_opcode(0);
            end if;

            if (cs = S_WAIT_INSTR) then
                cpl_tag <= '0';
            elsif (ld_tag = '1') then
                cpl_tag <= '1';
            end if;

            if (cs = S_WAIT_INSTR) then
                cpl_stat <= '0';
            elsif (ld_stat = '1') then
                cpl_stat <= '1';
            end if;

            if (cs = S_WAIT_INSTR) then
                msg_auth_done_r <= '0';
            elsif (msg_auth_done = '1') then
                msg_auth_done_r <= '1';
                msg_auth_valid_r <= msg_auth_valid;
            end if;

            if (en_len = '1') then
                if (unsigned(sgmt_len) < WB) then
                    reg_vbytes <= vbytes;
                else
                    reg_vbytes <= (others => '1');
                end if;
            end if;

            --! Keeping track of first data block (Only for special mode)
            if (G_CIPH_EXP) then
                if (ld_ciph_exp_len = '1') then
                    is_ciph_exp_len <= '1';
                elsif (en_len = '1' or cs = S_WAIT_INSTR) then
                    is_ciph_exp_len <= '0';
                end if;
                if (G_REVERSE_CIPH) then
                    if (set_first = '1') then
                        is_first <= '1';
                    elsif (clr_first = '1') then
                        clr_first <= '0';
                    end if;
                end if;
            end if;

            if (ld_sgmt_info = '1') then
                sgmt_type(3 downto 2) <= cmd_sgmt_type(3 downto 2);
                if (G_MERGE_TAG) then
                    sgmt_type(1 downto 0) <= not is_decrypt & '0';
                else
                    sgmt_type(1 downto 0) <= '0' & not is_decrypt;
                end if;
                sgmt_partial  <= cmd_sgmt_partial;
                sgmt_len      <= cmd_sgmt_len;
                sgmt_eot      <= cmd_sgmt_eot;
                sgmt_eoi      <= cmd_sgmt_eoi;
            elsif (ld_tag = '1') then
                sgmt_type     <= ST_TAG;
                sgmt_partial  <= '0';
                sgmt_eot      <= '1';
                sgmt_eoi      <= '1';
                sgmt_len      <= std_logic_vector(
                                to_unsigned(G_TAG_SIZE/8, sgmt_len'length));
            elsif (ld_stat = '1') then
                sgmt_type     <= STAT_SUCCESS(3 downto 1)
                                 & (not msg_auth_valid_r and is_decrypt);
                sgmt_partial  <= '0';
                sgmt_eot      <= '0';
                sgmt_eoi      <= '0';
                sgmt_len      <= (others => '0');
            elsif (en_len = '1') then
                if (sel_last = '1') then
                    sgmt_len <= (others => '0');
                else
                    sgmt_len <= std_logic_vector(unsigned(sgmt_len)-WB);
                end if;
            elsif (G_CIPH_EXP and ld_ciph_exp_len = '1') then
                sgmt_len <= ZEROS(15 downto G_LBS_BYTES+1) & bdo_size;
            end if;

            if (ld_ctr = '1') then
                ctr <= (others => '0');
            elsif (en_ctr = '1') then
                ctr <= std_logic_vector(unsigned(ctr)+1);
            end if;

            if (bdo_rdy = '1' and bdo_valid = '1') then
                reg_bdo <= bdo;
            elsif (en_ctr = '1') then
                reg_bdo <= reg_bdo(G_DBLK_SIZE-G_W-1 downto 0)
                    & ZEROS(G_W-1 downto 0);
            end if;
        end if;
    end process;

    --! Combinational logic of datapath
    gVbits:
    for i in WB-1 downto 0 generate
        reg_vbits(i*8+7 downto i*8) <= (others => reg_vbytes(i));
    end generate;
    word_size        <= sgmt_len(LOG2_WB-1 downto 0);
    vbytes <= VBYTES_LOOKUP(to_integer(unsigned(word_size)))
        when sel_last = '1'
        else (others => '1');

    sel_last <= '1' when (unsigned(sgmt_len) < WB) else '0';

    is_eoi <= '1' when (is_decrypt = '1'
                        and sgmt_type(3 downto 1) /= ST_NSEC
                        and sgmt_eot = '1')
                    or (is_decrypt = '0' and sgmt_type = ST_TAG)
                  else '0';
    out_hdr(G_W-1 downto G_W-32) <= sgmt_type
                & sgmt_partial & '0' & sgmt_eot & is_eoi
                & x"00" & sgmt_len;
    g_mt32:
    if (G_W > 32) generate
        out_hdr(G_W-33 downto 0) <= (others => '0');
    end generate;
        
    gBuffer: if (IS_BUFFER) generate
        out_word <= reg_bdo(G_DBLK_SIZE-1 downto G_DBLK_SIZE-G_W) and reg_vbits;
    end generate;
    gNotBuffer: if (not IS_BUFFER) generate
        out_word <= bdo and reg_vbits;
    end generate;
    do_data <= out_hdr when sel_do = '1' else out_word;

    --! Output communication
    cmd_ready <= cmd_rdy;
    gOutBuffer:
    if (IS_BUFFER) generate
        bdo_ready <= reg_bdo_ready;
    end generate;
    gOutNotBuffer:
    if (not IS_BUFFER) generate
        bdo_ready <= bdo_rdy;
    end generate;
    do_valid  <= do_vld;

    --! Command FIFO dissection
    cmd_instr_opcode <= cmd(24-1 downto 24-4);
    cmd_sgmt_type    <= cmd(24-1 downto 24-4);
    gCiphExp:
    if (G_CIPH_EXP) generate
        cmd_sgmt_partial <= cmd(24-5);
    end generate;
    gNotCiphExp:
    if (not G_CIPH_EXP) generate
        is_ciph_exp_len  <= '0';
        ld_ciph_exp_len  <= '0';
        cmd_sgmt_partial <= '0';
    end generate;
    cmd_sgmt_eot <= cmd(24-7);
    cmd_sgmt_eoi <= cmd(24-8);
    cmd_sgmt_len <= cmd(24-9 downto 24-24);


    --! =======================================================================
    --! Control
    --! =======================================================================
    --! State transition
    gNotAsync:
    if (not G_ASYNC_RSTN) generate
        process(clk)
        begin
            if rising_edge(clk) then
                if (rst = '1') then
                    cs <= S_WAIT_INSTR;
                    reg_bdo_ready <= '0';
                else
                    if (en_s = '1') then
                        cs <= ns;
                    end if;

                    --! BDO ready register
                    if (en_s = '1'
                        and (ns = S_WAIT_BDO 
                            or (G_CIPH_EXP and ns = S_WAIT_BDO_CIPH)))
                    then
                        reg_bdo_ready <= '1';
                    elsif (bdo_valid = '1' and reg_bdo_ready = '1') then
                        reg_bdo_ready <= '0';
                    end if;
                end if;
            end if;
        end process;
    end generate;
    gAsync:
    if (G_ASYNC_RSTN) generate
        process(clk, rst)
        begin
            if (rst = '0') then
                cs <= S_WAIT_INSTR;
                reg_bdo_ready <= '0';
            elsif rising_edge(clk) then
                if (en_s = '1') then
                    cs <= ns;
                end if;
                --! BDO ready register
                if (en_s = '1'
                    and (ns = S_WAIT_BDO 
                            or (G_CIPH_EXP and ns = S_WAIT_BDO_CIPH)))
                then
                    reg_bdo_ready <= '1';
                elsif (bdo_valid = '1' and reg_bdo_ready = '1') then
                    reg_bdo_ready <= '0';
                end if;
            end if;
        end process;
    end generate;

    --! Combinational logic
    gPdiComb:
    process(cs, cmd_instr_opcode,
        cmd_sgmt_type, cmd_sgmt_eot, cmd_sgmt_eoi, cmd_sgmt_len,
        is_decrypt, do_ready, bdo_valid,
        cmd_valid, ctr, sgmt_len, sgmt_eoi, sgmt_eot,
        is_first, is_ciph_exp_len,
        cpl_tag, cpl_stat, msg_auth_done_r)
    begin
        ns           <= cs;
        cmd_rdy      <= '0';
        bdo_rdy      <= '0';
        do_vld       <= '0';
        en_ctr       <= '0';
        en_len       <= '0';
        en_s         <= '0';
        ld_ctr       <= '0';
        ld_sgmt_info <= '0';
        ld_stat      <= '0';
        ld_tag       <= '0';
        sel_do       <= '0';
        if (G_CIPH_EXP) then
            ld_ciph_exp_len <= '0';
            if (G_REVERSE_CIPH) then
                clr_first   <=  '1';
            end if;
        end if;

        case cs is
            when S_WAIT_INSTR =>
                ns      <= S_WAIT_HDR;
                cmd_rdy <= '1';
                if (cmd_valid = '1'
                    and cmd_instr_opcode(3 downto 1) = OP_ENCDEC)
                then
                    en_s <= '1';
                end if;
                if (G_CIPH_EXP and G_REVERSE_CIPH) then
                    set_first <= '1';
                end if;

            when S_WAIT_HDR =>
                ld_sgmt_info <= '1';
                cmd_rdy      <= '1';
                if (G_CIPH_EXP) then
                    if (cmd_sgmt_type(3 downto 2) = ST_D
                        and ((G_REVERSE_CIPH and is_first = '1')
                            or (cmd_sgmt_eot = '1')))
                    then
                        ns <= S_WAIT_BDO_CIPH;
                        if (G_REVERSE_CIPH) then
                            clr_first <= '1';
                        end if;
                    else
                        ns <= S_PREP;
                    end if;
                else
                    ns           <= S_PREP;
                end if;
                if (cmd_valid = '1') then
                    en_s   <= '1';
                end if;

            --! Prepare appropriate flags and generate output header/status
            when S_PREP =>
                do_vld <= '1';
                if (do_ready = '1') then
                    if (cpl_stat = '1') then
                        ns <= S_WAIT_INSTR;
                    else
                        if (unsigned(sgmt_len) > 0)
                            or G_MERGE_TAG
                        then
                            if (not IS_BUFFER) then
                                ns <= S_OUT;
                            else
                                if (G_CIPH_EXP and is_ciph_exp_len = '1') then
                                    ns <= S_OUT;
                                else
                                    ns <= S_WAIT_BDO;
                                end if;
                            end if;
                        else
                            if (sgmt_eot = '1'
                                and sgmt_type(3 downto 2) = ST_D)
                            then
                                if (is_decrypt = '0') then
                                    ns <= S_GEN_TAG_HDR;
                                else
                                    ns <= S_GEN_STAT_HDR;
                                end if;
                            end if;
                        end if;
                    end if;
                    en_s   <= '1';
                    en_len <= '1';
                end if;
                sel_do <= '1';

            --! Output data
            when S_OUT =>
                if (not IS_BUFFER) then
                    bdo_rdy <= '1';
                    if (do_ready = '1' and bdo_valid = '1') then
                        do_vld <= '1';
                        en_len <= '1';
                        en_s   <= '1';
                    end if;
                else
                    do_vld <= '1';
                    if (do_ready = '1') then
                        en_len <= '1';
                        en_ctr <= '1';
                        en_s   <= '1';
                    end if;
                end if;

                if (unsigned(sgmt_len) = 0) then
                    if (sgmt_eot = '1') then
                        if (is_decrypt = '0' and cpl_tag = '0') then
                            ns <= S_GEN_TAG_HDR;
                        else
                            ns <= S_GEN_STAT_HDR;
                        end if;
                    else
                        ns <= S_WAIT_HDR;
                    end if;
                elsif (G_W /= G_DBLK_SIZE
                    and unsigned(ctr) = CNT_DWORDS-1)
                then
                    ns <= S_WAIT_BDO;
                end if;

            when S_GEN_TAG_HDR =>
                ld_tag  <= '1';
                ns      <= S_PREP;
                en_s    <= '1';

            when S_GEN_STAT_HDR =>
                ld_stat <= '1';
                ns      <= S_PREP;
                if (is_decrypt = '0' or msg_auth_done_r = '1') then
                    en_s    <= '1';
                end if;

            when S_WAIT_BDO =>
                ns      <= S_OUT;
                bdo_rdy <= '1';
                ld_ctr  <= '1';
                if (bdo_valid = '1') then
                    en_s <= '1';
                end if;

            when S_WAIT_BDO_CIPH =>
                ns              <= S_PREP;
                ld_ciph_exp_len <= '1';
                bdo_rdy         <= '1';
                ld_ctr          <= '1';
                if (bdo_valid = '1') then
                    en_s <= '1';
                end if;


        end case;
    end process;
end structure;
