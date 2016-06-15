-------------------------------------------------------------------------------
--! @file       PreProcessor.vhd
--! @brief      Pre-processing unit for an authenticated encryption module.
--! @project    CAESAR Candidate Evaluation
--! @author     Ekawat (ice) Homsirikamol
--! @copyright  Copyright (c) 2015 Cryptographic Engineering Research Group
--!             ECE Department, George Mason University Fairfax, VA, U.S.A.
--!             All rights Reserved.
--! @license    This project is released under the GNU Public License.
--!             The license and distribution terms for this file may be
--!             found in the file LICENSE in this distribution or at
--!             http://www.gnu.org/licenses/gpl-3.0.txt
--! @note       This is publicly available encryption source code that falls
--!             under the License Exception TSU (Technology and software-
--!             —unrestricted)
--! SIPO used within this unit follows the following convention:
--! > Order in the test vector file (left to right):  A(0) A(1) A(2) … A(N-1)
--! > Order at the SIPO input (time 0 to time N-1) :  A(0) A(1) A(2) … A(N-1)
--! > Order at the SIPO output (left to right)     :  A(0) A(1) A(2) … A(N-1)
--! where A is a single I/O word.
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.AEAD_pkg.all;

entity PreProcessor is
    generic (
        --! I/O size (bits)
        G_W                 : integer := 32;    --! Public data input
        G_SW                : integer := 32;    --! Secret data input
        --! Reset behavior
        G_ASYNC_RSTN        : boolean := False; --! Async active low reset
        --! Special features activation
        G_ENABLE_PAD        : boolean := False; --! Enable padding
        G_CIPH_EXP          : boolean := False; --! Ciphertext expansion
        G_REVERSE_CIPH      : boolean := False; --! Reversed ciphertext
        G_MERGE_TAG         : boolean := False; --! Merge tag with data segment
        --! Block size (bits)
        G_ABLK_SIZE         : integer := 128;   --! Associated data
        G_DBLK_SIZE         : integer := 128;   --! Data
        G_KEY_SIZE          : integer := 128;   --! Key
        --! The number of bits required to hold block size expressed in
        --! bytes = log2_ceil(G_DBLK_SIZE/8)
        G_LBS_BYTES         : integer := 4;
        --! Padding options
        G_PAD_STYLE         : integer := 0;     --! Pad style
        G_PAD_AD            : integer := 1;     --! Padding behavior for AD
        G_PAD_D             : integer := 1      --! Padding behavior for Data
    );
    port (
        --! Global ports
        clk                 : in  std_logic;
        rst                 : in  std_logic;
        --! Publica data ports
        pdi_data            : in  std_logic_vector(G_W          -1 downto 0);
        pdi_valid           : in  std_logic;
        pdi_ready           : out std_logic;
        --! Secret data ports
        sdi_data            : in  std_logic_vector(G_SW         -1 downto 0);
        sdi_valid           : in  std_logic;
        sdi_ready           : out std_logic;
        --! CipherCore
        --!     Key
        key                 : out std_logic_vector(G_KEY_SIZE   -1 downto 0);
        key_ready           : in  std_logic;
        key_valid           : out std_logic;
        key_update          : out std_logic;
        --!     BDI
        bdi                 : out std_logic_vector(G_DBLK_SIZE  -1 downto 0);
        decrypt             : out std_logic;
        bdi_ready           : in  std_logic;
        bdi_valid           : out std_logic;
        bdi_type            : out std_logic_vector(3            -1 downto 0);
        bdi_partial         : out std_logic;
        bdi_eot             : out std_logic;
        bdi_eoi             : out std_logic;
        bdi_size            : out std_logic_vector(G_LBS_BYTES+1-1 downto 0);
        bdi_valid_bytes     : out std_logic_vector(G_DBLK_SIZE/8-1 downto 0);
        bdi_pad_loc         : out std_logic_vector(G_DBLK_SIZE/8-1 downto 0);
        --! CMD FIFO
        cmd                 : out std_logic_vector(24           -1 downto 0);
        cmd_ready           : in  std_logic;
        cmd_valid           : out std_logic
    );
end entity PreProcessor;

architecture structure of PreProcessor is
    constant DSIZE          : integer := G_DBLK_SIZE;
    constant ASIZE          : integer := G_ABLK_SIZE;
    constant WB             : integer := G_W/8; --! Word bytes
    constant LOG2_WB        : integer := log2_ceil(WB);
    constant LOG2_KEYBYTES  : integer := log2_ceil(512/8);
    constant CNT_AWORDS     : integer := (G_ABLK_SIZE+(G_W-1))/G_W;
    constant CNT_DWORDS     : integer := (G_DBLK_SIZE+(G_W-1))/G_W;
    constant CNT_KWORDS     : integer := (G_KEY_SIZE+(G_SW-1))/G_SW;
    constant A_EQ_D         : boolean := (DSIZE = ASIZE);
    constant P_IS_BUFFER    : boolean := not (G_W = DSIZE);
    constant S_IS_BUFFER    : boolean := not (G_SW = G_KEY_SIZE);
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
    function getPlocLookup(size: integer) return t_lookup is
        variable ret : t_lookup;
    begin
      
        for i in 0 to ((size/8)-1) loop
          ret(i) := (others => '0');
          ret(i)((size/8-i)-1) := '1';
          --ret(i) := (((size/8-i)-1) => '1', others => '0');
        end loop;
        return ret;
    end function getPlocLookup;
    constant VBYTES_LOOKUP  : t_lookup := getVbytesLookup(G_W);
    constant PLOC_LOOKUP    : t_lookup := getPlocLookup(G_W);
    --! =======================================================================
    --! Control status registers
    --!     Public
    signal sgmt_type        : std_logic_vector(4                -1 downto 0);
    signal sgmt_pt          : std_logic;
    signal sgmt_eoi         : std_logic;
    signal sgmt_eot         : std_logic;
    signal sgmt_lst         : std_logic;
    signal sgmt_len         : std_logic_vector(16               -1 downto 0);
    signal is_decrypt       : std_logic;
    --!     Secret
    signal reg_key_update   : std_logic;
    signal reg_key_valid    : std_logic;
    --! =======================================================================
    --! Control signals
    --!     Pad
    signal set_extra        : std_logic;
    signal set_req_pad      : std_logic;
    signal req_pad          : std_logic;
    signal is_extra         : std_logic;
    signal sel_pad          : std_logic;
    signal is_pad           : std_logic;
    signal en_len           : std_logic;
    signal en_zero          : std_logic;
    signal reg_sel_zero     : std_logic;
    --!     Public
    signal pdi_rdy          : std_logic;
    signal bdi_vld          : std_logic;
    signal set_key_upd      : std_logic;
    signal ld_sgmt_info     : std_logic;
    signal ld_ctr           : std_logic;
    signal en_ctr           : std_logic;
    signal en_ps            : std_logic;
    signal en_data          : std_logic;
    signal ctr              : std_logic_vector
        (log2_ceil(CNT_DWORDS)-1 downto 0);
    signal sel_end          : std_logic;
    signal ld_end           : std_logic;
    --!     (unused)
    signal en_last_word     : std_logic;

    --!     Secret
    signal sdi_rdy          : std_logic;
    signal ld_ctr2          : std_logic;
    signal ld_slen          : std_logic;
    signal en_ctr2          : std_logic;
    signal en_slen          : std_logic;
    signal en_ss            : std_logic;
    signal en_key           : std_logic;
    signal slen             : std_logic_vector(LOG2_KEYBYTES+1  -1 downto 0);
    signal ctr2             : std_logic_vector
        (log2_ceil(CNT_KWORDS)-1 downto 0);
    --!     Cmd
    signal wr_cmd           : std_logic;
    --! =======================================================================
    --! State
    type t_ps is (S_WAIT_INSTR, S_WAIT_HDR, S_PREP, S_DATA, S_WAIT_READY);
    type t_ss is (S_WAIT_INSTR, S_WAIT_HDR, S_DATA, S_WAIT_READY);
    signal ps               : t_ps; --! Public State
    signal nps              : t_ps; --! Next Public State
    signal ss               : t_ss; --! Next Secret State
    signal nss              : t_ss; --! Next Secret State
    --! =======================================================================
    --! Data padding
    signal word_size        : std_logic_vector(LOG2_WB          -1 downto 0);
    signal data             : std_logic_vector(G_W              -1 downto 0);
    --!     Incoming data word
    signal pdata            : std_logic_vector(G_W              -1 downto 0);
    signal vbytes           : std_logic_vector(WB               -1 downto 0);
    signal ploc             : std_logic_vector(WB               -1 downto 0);
    --!     Additional padding selection when ASIZE /= DSIZE
    signal pdata2           : std_logic_vector(G_W              -1 downto 0);
    signal vbytes2          : std_logic_vector(WB               -1 downto 0);
    signal ploc2            : std_logic_vector(WB               -1 downto 0);

    --! Output regs
    --!     Prep status
    signal mux_vbytes       : std_logic_vector(WB               -1 downto 0);
    signal mux_ploc         : std_logic_vector(WB               -1 downto 0);
    signal mux_size         : std_logic_vector(LOG2_WB+1        -1 downto 0);
    signal size             : std_logic_vector(LOG2_WB+1        -1 downto 0);
    --!     Status
    signal reg_bdi_valid    : std_logic;
    signal reg_size         : std_logic_vector(G_LBS_BYTES+1    -1 downto 0);
    signal reg_vbytes       : std_logic_vector(G_DBLK_SIZE/8    -1 downto 0);
    signal reg_ploc         : std_logic_vector(G_DBLK_SIZE/8    -1 downto 0);
    --!     Data / info
    signal reg_key          : std_logic_vector(G_KEY_SIZE       -1 downto 0);
    signal reg_data         : std_logic_vector(G_DBLK_SIZE      -1 downto 0);
    --! =======================================================================
    --! Signal aliases
    signal p_instr_opcode   : std_logic_vector(4                -1 downto 0);
    signal p_sgmt_type      : std_logic_vector(4                -1 downto 0);
    signal p_sgmt_pt        : std_logic;
    signal p_sgmt_eoi       : std_logic;
    signal p_sgmt_eot       : std_logic;
    signal p_sgmt_lst       : std_logic;
    signal p_sgmt_len       : std_logic_vector(16               -1 downto 0);
    signal s_instr_opcode   : std_logic_vector(4                -1 downto 0);
    signal s_sgmt_type      : std_logic_vector(4                -1 downto 0);
    signal s_sgmt_eot       : std_logic;
    signal s_sgmt_lst       : std_logic;
    signal s_sgmt_len       : std_logic_vector(LOG2_KEYBYTES+1   -1 downto 0);
begin
    --! =======================================================================
    --! Datapath (Core)
    --! =======================================================================
    data    <= pdi_data when reg_sel_zero = '0' else (others => '0');

    gPad0: if (not G_ENABLE_PAD) generate
        pdata <= data;
    end generate;
    gPad1: if (G_ENABLE_PAD) generate
    begin
        gPadMode0: if (G_PAD_STYLE = 0) generate
            pdata <= data;
        end generate;
        gPadMode1: if (G_PAD_STYLE = 1) generate
            gLoop: for i in WB-1 downto 0 generate
                pdata(i*8+7)
                    <= ploc(i) or data(i*8+7);
                pdata(i*8+6 downto i*8)
                    <= data(i*8+6 downto i*8);
            end generate;
        end generate;
    end generate;

    mux_vbytes <= VBYTES_LOOKUP(to_integer(unsigned(word_size)))
        when sel_pad = '1'
        else (others => '1');
    mux_ploc <= PLOC_LOOKUP(to_integer(unsigned(word_size)))
        when (sel_pad = '1' and req_pad = '1')
        else (others => '0');
    mux_size <= '0' & word_size
        when sel_pad = '1'
        else (LOG2_WB => '1', others => '0');

    process(clk)
    begin
        if rising_edge(clk) then
            if (en_len = '1') then
                vbytes <= mux_vbytes;
                ploc   <= mux_ploc;
                size   <= mux_size;
            end if;

            if (en_data = '1') then
                if ((DSIZE > G_W) and (DSIZE MOD G_W)  = 0)  then
                    reg_data  <= reg_data(DSIZE-G_W-1 downto 0) & pdata;
                    reg_vbytes<= reg_vbytes(DSIZE/8-WB-1 downto 0) & vbytes;
                    reg_ploc  <= reg_ploc  (DSIZE/8-WB-1 downto 0) & ploc;
                elsif ((DSIZE MOD G_W) /= 0) then
                    if (en_last_word = '0') then
                        reg_data  (DSIZE-1   downto (DSIZE MOD G_W )) <=
                            reg_data(DSIZE-G_W-1 downto (DSIZE MOD G_W))
                            & pdata2;
                        reg_vbytes(DSIZE/8-1 downto ((DSIZE/8) MOD WB)) <=
                            reg_vbytes(DSIZE/8-WB-1 downto ((DSIZE/8) MOD WB))
                            & vbytes2;
                        reg_ploc(DSIZE/8-1 downto ((DSIZE/8) MOD WB)) <=
                            reg_ploc(DSIZE/8-WB-1 downto ((DSIZE/8) MOD WB))
                            & ploc2;
                    else
                        reg_data  ((DSIZE mod G_W)-1 downto 0) <=
                            pdata2(G_W  -1 downto  G_W   /2);
                        reg_vbytes(((DSIZE/8) mod WB)-1 downto 0) <=
                            vbytes(WB-1 downto WB/2);
                        reg_ploc(((DSIZE/8) mod WB)-1 downto 0) <=
                            ploc2(WB-1 downto WB/2);
                    end if;
                end if;
            end if;

            if (en_key = '1') then
                if (G_SW < G_KEY_SIZE) then
                    reg_key <= reg_key(G_KEY_SIZE-G_SW-1 downto 0) & sdi_data;
                end if;
            end if;
        end if;
    end process;

    --! =======================================================================
    --! Registers with rst for controller and datapath
    --! =======================================================================
    gSyncRst:
    if (not G_ASYNC_RSTN) generate
        process(clk)
        begin
            if rising_edge(clk) then
                if (rst = '1') then
                    --! Datapath
                    reg_size       <= (others => '0');
                    reg_bdi_valid  <= '0';
                    reg_key_update <= '0';
                    reg_key_valid  <= '0';
                    --! Control
                    req_pad        <= '0';
                    ps             <= S_WAIT_INSTR;
                    ss             <= S_WAIT_INSTR;
                else
                    --! Datapath
                    if (en_data = '1') then
                        reg_size <= std_logic_vector(
                            unsigned(reg_size) + unsigned(size));
                    elsif (bdi_ready = '1') then
                        reg_size <= (others => '0');
                    end if;
                    --! BDI valid register
                    if (en_ps = '1' and nps = S_WAIT_READY) then
                        reg_bdi_valid <= '1';
                    elsif (reg_bdi_valid = '1' and bdi_ready = '1') then
                        reg_bdi_valid <= '0';
                    end if;
                    --! Key update register
                    if (set_key_upd = '1') then
                        reg_key_update <= '1';
                    elsif (key_ready = '1'
                            and ((S_IS_BUFFER and reg_key_valid = '1')
                                or (not S_IS_BUFFER and sdi_valid = '1')))
                    then
                        reg_key_update <= '0';
                    end if;
                    --! Key valid register
                    if (en_ss = '1' and nss = S_WAIT_READY) then
                        reg_key_valid <= '1';
                    elsif (key_ready = '1' and reg_key_valid = '1') then
                        reg_key_valid <= '0';
                    end if;
                    --! Control
                    if (set_req_pad = '1') then
                        req_pad <= '1';
                    elsif (en_len = '1' and sel_pad = '1')
                        or ps = S_WAIT_INSTR
                    then
                        req_pad <= '0';
                    end if;
                    if (en_ps = '1') then
                        ps <= nps;
                    end if;
                    if (en_ss = '1') then
                        ss <= nss;
                    end if;
                end if;
            end if;
        end process;
    end generate;
    gAsyncRstn:
    if (G_ASYNC_RSTN) generate
        process(clk, rst)
        begin
            if (rst = '0') then
                --! Datapath
                reg_size       <= (others => '0');
                reg_bdi_valid  <= '0';
                reg_key_update <= '0';
                reg_key_valid  <= '0';
                --! Control
                req_pad        <= '0';
                ps             <= S_WAIT_INSTR;
                ss             <= S_WAIT_INSTR;
            elsif rising_edge(clk) then
                --! Datapath
                if (en_data = '1') then
                    reg_size <= std_logic_vector(
                        unsigned(reg_size) + unsigned(size));
                elsif (bdi_ready = '1') then
                    reg_size <= (others => '0');
                end if;
                --! BDI valid register
                if (en_ps = '1' and nps = S_WAIT_READY) then
                    reg_bdi_valid <= '1';
                elsif (reg_bdi_valid = '1' and bdi_ready = '1') then
                    reg_bdi_valid <= '0';
                end if;
                --! Key update register
                if (set_key_upd = '1') then
                    reg_key_update <= '1';
                elsif (key_ready = '1'
                        and ((S_IS_BUFFER and reg_key_valid = '1')
                            or (not S_IS_BUFFER and sdi_valid = '1')))
                then
                    reg_key_update <= '0';
                end if;
                --! Key valid register
                if (en_ss = '1' and nss = S_WAIT_READY) then
                    reg_key_valid <= '1';
                elsif (key_ready = '1' and reg_key_valid = '1') then
                    reg_key_valid <= '0';
                end if;
                --! Control
                if (set_req_pad = '1') then
                    req_pad <= '1';
                elsif (en_len = '1' and sel_pad = '1')
                    or ps = S_WAIT_INSTR
                then
                    req_pad <= '0';
                end if;
                if (en_ps = '1') then
                    ps <= nps;
                end if;
                if (en_ss = '1') then
                    ss <= nss;
                end if;
            end if;
        end process;
    end generate;

    --! =======================================================================
    --! Datapath (Output)
    --! =======================================================================
    pdi_ready <= pdi_rdy;
    sdi_ready <= sdi_rdy;
    --!     Public
    decrypt     <= is_decrypt;
    gDsizeEq:
    if (not P_IS_BUFFER) generate
        bdi             <= pdata;
        bdi_vld         <= pdi_valid when (ps = S_DATA) else '0';
        bdi_type        <= sgmt_type(3 downto 1);
        gNotCiph:
        if (not G_CIPH_EXP) generate
            bdi_eot         <= sgmt_eot
                when (ps = S_DATA and unsigned(sgmt_len) = 0)
                else '0';
            bdi_eoi         <= sgmt_eoi
                when (ps = S_DATA and unsigned(sgmt_len) = 0)
                else '0';
        end generate;
        gCiph:
        if (G_CIPH_EXP) generate
            bdi_eot         <= sgmt_eot or sgmt_eoi
                when (ps = S_DATA and unsigned(sgmt_len) = 0)
                else '0';
            bdi_eoi         <= sgmt_lst
                when (ps = S_DATA and unsigned(sgmt_len) = 0)
                else '0';
            bdi_partial     <= sgmt_pt;
        end generate;
        bdi_size        <= size;
        bdi_valid_bytes <= vbytes;
        bdi_pad_loc     <= ploc;
    end generate;
    gDsizeNeq:
    if (P_IS_BUFFER) generate
        signal en_eoi_last : std_logic;
        signal en_eot_last : std_logic;
    begin
        bdi             <= reg_data;
        bdi_vld         <= reg_bdi_valid;
        bdi_type        <= sgmt_type(3 downto 1);
        pEnd:
        process(clk)
        begin
            if rising_edge(clk) then
                if (ld_end = '1') then
                    if (not G_CIPH_EXP) then
                        bdi_eot <= sgmt_eot and sel_end;
                        bdi_eoi <= sgmt_eoi and sel_end;
                    else
                        bdi_eot <= (sgmt_eot or en_eot_last) and sel_end;
                        bdi_eoi <= (sgmt_eoi or en_eoi_last) and sel_end;
                    end if;
                end if;
            end if;
        end process;
        gCiph:
        if (G_CIPH_EXP) generate
            en_eot_last  <= '1' when
                                ((sgmt_eoi = '1' and sgmt_type = ST_NPUB)
                                or (sgmt_eoi = '1' and G_ENABLE_PAD
                                    and sgmt_type(3 downto 2) = ST_A
                                    and G_PAD_AD /= 2 and G_PAD_AD /= 4)
                                or (sgmt_eoi = '1' and G_ENABLE_PAD
                                    and sgmt_type(3 downto 2) = ST_D
                                    and G_PAD_D /= 2  and G_PAD_D /= 4))
                                else '0';
            en_eoi_last  <= '1' when en_eot_last = '1' and is_decrypt = '0'
                                else '0';
            bdi_partial  <= sgmt_pt;
        end generate;
        bdi_size        <= reg_size;
        bdi_valid_bytes <= reg_vbytes;
        bdi_pad_loc     <= reg_ploc;
    end generate;
    bdi_valid       <= bdi_vld;

    --!     Secret
    gTsizeEq:
    if (S_IS_BUFFER) generate
        key_valid   <= reg_key_valid;
        key         <= reg_key;
    end generate;
    gTsizeNeq:
    if (not S_IS_BUFFER) generate
        key_valid   <= sdi_valid when (ss = S_DATA) else '0';
        key         <= sdi_data;
    end generate;
    key_update  <= reg_key_update;
    --!     CMD FIFO
    cmd       <= pdi_data(G_W-1 downto G_W-5) & '0'
                 & pdi_data(G_W-7 downto G_W-8)
                 & pdi_data(G_W-17 downto G_W-32);
    cmd_valid <= wr_cmd;


    --! =======================================================================
    --! Control
    --! =======================================================================
    process(clk)
    begin
        if rising_edge(clk) then
            --! Operation register
            if (ps = S_WAIT_INSTR) then
                is_decrypt <= p_instr_opcode(0);
            end if;
            --! Length register
            if (ld_sgmt_info = '1') then
                sgmt_type <= p_sgmt_type;
                if (G_CIPH_EXP) then
                    sgmt_pt   <= p_sgmt_pt;
                end if;
                sgmt_eoi  <= p_sgmt_eoi;
                sgmt_eot  <= p_sgmt_eot;
                sgmt_lst  <= p_sgmt_lst;
                sgmt_len  <= p_sgmt_len;
            else
                if (en_len  = '1') then
                    if (sel_pad = '1') then
                        sgmt_len <= (others => '0');
                    else
                        sgmt_len <= std_logic_vector(unsigned(sgmt_len)-WB);
                    end if;
                end if;
            end if;
            --! Padding activation register
            if (en_len = '1') then
                is_pad <= sel_pad;
            end if;
            --! Select zero register
            if (ld_sgmt_info = '1')
                or (P_IS_BUFFER and not A_EQ_D
                    and bdi_ready = '1' and unsigned(sgmt_len) > 0)
            then
                reg_sel_zero <= '0';
            elsif  (unsigned(sgmt_len) = 0 and en_len = '1')
                    or (not A_EQ_D and en_zero = '1')
            then
                reg_sel_zero <= '1';
            end if;
            --! Secret length register
            if (ld_slen = '1') then
                slen <= s_sgmt_len;
            elsif (en_slen = '1') then
                slen <= std_logic_vector(unsigned(slen)-G_KEY_SIZE/8);
            end if;
            --! Extra block register
            if (ld_sgmt_info = '1' or (bdi_ready = '1' and bdi_vld = '1')) then
                is_extra <= '0';
            elsif (set_extra = '1') then
                is_extra <= '1';
            end if;
            --! Public data input counter register
            if (ld_ctr = '1') then
                ctr <= (others => '0');
            elsif (en_ctr = '1') then
                ctr <= std_logic_vector(unsigned(ctr) + 1);
            end if;
            --! Secret data input counter register
            if (ld_ctr2 = '1') then
                ctr2 <= (others => '0');
            elsif (en_ctr2 = '1') then
                ctr2 <= std_logic_vector(unsigned(ctr2) + 1);
            end if;
        end if;
    end process;

    sel_pad <= '1' when (unsigned(sgmt_len) < WB) else '0';

    word_size      <= sgmt_len(LOG2_WB-1 downto 0);
    --! HDR Dissection
    p_instr_opcode <= pdi_data(G_W-1 downto G_W-4);
    p_sgmt_type    <= pdi_data(G_W-1 downto G_W-4);
    p_sgmt_pt      <= pdi_data(G_W-5);
    p_sgmt_eoi     <= pdi_data(G_W-6);
    p_sgmt_eot     <= pdi_data(G_W-7);
    p_sgmt_lst     <= pdi_data(G_W-8);
    p_sgmt_len     <= pdi_data(G_W-17 downto G_W-32);
    s_instr_opcode <= sdi_data(G_SW-1 downto G_SW-4);
    s_sgmt_type    <= sdi_data(G_SW-1 downto G_SW-4);
    s_sgmt_eot     <= sdi_data(G_SW-7);
    s_sgmt_lst     <= sdi_data(G_SW-8);
    s_sgmt_len     <= sdi_data(G_SW-32+LOG2_KEYBYTES downto G_SW-32);

    gPdiComb:
    process(ps, p_instr_opcode, pdi_valid,
        sgmt_len, sgmt_type, sgmt_eot, sgmt_lst,
        p_sgmt_eot, p_sgmt_type,
        bdi_ready, cmd_ready, reg_sel_zero,
        is_extra, ctr)
    begin
        nps          <= ps;
        pdi_rdy      <= '1';
        set_key_upd  <= '0';
        set_req_pad  <= '0';

        ld_sgmt_info <= '0';
        if (P_IS_BUFFER) then
            ld_end       <= '0';
            set_extra    <= '0';
        end if;
        ld_ctr       <= '0';
        en_data      <= '0';
        en_ps        <= '0';
        en_len       <= '0';
        en_ctr       <= '0';
        en_zero      <= '0';
        wr_cmd       <= '0';

        case ps is
            when S_WAIT_INSTR =>
                ld_ctr      <= '1';
                if (p_instr_opcode(3 downto 1) = OP_ENCDEC) then
                    nps      <= S_WAIT_HDR;
                end if;
                if (p_instr_opcode = OP_ACTKEY) then
                    set_key_upd <= '1';
                end if;
                if (cmd_ready = '0') then
                    pdi_rdy <= '0';
                end if;
                if (pdi_valid = '1') then
                    en_ps  <= '1';
                    wr_cmd <= '1';
                end if;

            when S_WAIT_HDR =>
                ld_sgmt_info <= '1';
                nps          <= S_PREP;
                if (cmd_ready = '0') then
                    pdi_rdy <= '0';
                end if;
                if (pdi_valid = '1' and cmd_ready = '1') then
                    en_ps  <= '1';
                    if (p_sgmt_type(3 downto 2) = ST_D
                        or p_sgmt_type(3 downto 1) = ST_NSEC)
                    then
                        wr_cmd <= '1';
                    end if;
                end if;
                if (G_ENABLE_PAD) then
                    if (p_sgmt_eot = '1') then
                        if (p_sgmt_type(3 downto 2) = ST_A and G_PAD_AD > 0)
                            or (p_sgmt_type(3 downto 2) = ST_D and G_PAD_D > 0)
                        then
                            set_req_pad <= '1';
                        end if;
                    end if;
                end if;

            when S_PREP =>
                pdi_rdy <= '0';
                --! state transition
                if (unsigned(sgmt_len) = 0) then
                    if (G_ENABLE_PAD) and
                        --! Add a new block based on padding behavior
                        ((sgmt_type(3 downto 2) = ST_A
                            and (G_PAD_AD = 2 or G_PAD_AD = 4))
                        or (sgmt_type(3 downto 2) = ST_D
                            and (G_PAD_D = 2 or G_PAD_D = 4)))
                    then
                        nps <= S_DATA;
                    else
                        if (sgmt_lst = '1') then
                            nps <= S_WAIT_INSTR;
                        else
                            nps <= S_WAIT_HDR;
                        end if;
                    end if;
                else
                    nps    <= S_DATA;
                end if;
                en_len <= '1';
                en_ps  <= '1';

            when S_DATA =>
                if (not P_IS_BUFFER) then
                    --! Without buffer
                    if (reg_sel_zero = '1'
                        or (not P_IS_BUFFER
                            and (pdi_valid = '0' or bdi_ready = '0')))
                    then
                        pdi_rdy <= '0';
                    end if;

                    if (unsigned(sgmt_len) = 0)
                        and not (req_pad = '1' and G_ENABLE_PAD
                                and ((sgmt_type(3 downto 2) = ST_A and G_PAD_AD > 2)
                                    or (sgmt_type(3 downto 2) = ST_D and G_PAD_D > 2)))
                    then
                        if (sgmt_lst = '1') then
                            nps <= S_WAIT_INSTR;
                        else
                            nps <= S_WAIT_HDR;
                        end if;
                    end if;
                else
                    --! With buffer
                    if (reg_sel_zero = '1') then
                        pdi_rdy <= '0';
                    end if;

                    if (unsigned(ctr) = CNT_DWORDS-1) then
                        nps <= S_WAIT_READY;
                    end if;
                    if (unsigned(ctr) = CNT_DWORDS-2) then
                        ld_end <= '1';
                    end if;
                    if (unsigned(sgmt_len) = WB and G_ENABLE_PAD
                        and sgmt_eot = '1'
                        and ((sgmt_type(3 downto 2) = ST_A and G_PAD_AD > 2)
                            or (sgmt_type(3 downto 2) = ST_D
                                and G_PAD_D > 2
                                and (not G_CIPH_EXP
                                    or (G_CIPH_EXP and is_decrypt = '0')))))
                    then
                        if (A_EQ_D) then
                            if unsigned(ctr) = CNT_DWORDS-2 then
                                set_extra <= '1';
                            end if;
                        else
                            if ((sgmt_type(3 downto 2) = ST_A
                                    and unsigned(ctr) = CNT_AWORDS-2)
                                or (sgmt_type(3 downto 2) = ST_D
                                    and unsigned(ctr) = CNT_DWORDS-2))
                            then
                                set_extra <= '1';
                            end if;
                        end if;
                    end if;

                    --! if ASIZE < DSIZE
                    if (not A_EQ_D) then
                        if (sgmt_type(3 downto 2) = ST_A
                            and unsigned(ctr) >= CNT_AWORDS-1)
                        then
                            en_zero <= '1';
                        end if;
                    end if;
                end if;

                if (reg_sel_zero = '1'
                    or (pdi_valid = '1'
                        and (P_IS_BUFFER
                            or (not P_IS_BUFFER and bdi_ready = '1'))))
                then
                    if (sgmt_type(3 downto 2) /= ST_A
                        and sgmt_type(3 downto 2) /= ST_D)
                    then
                        --! Not AD or D segment
                        if (P_IS_BUFFER) then
                            if (unsigned(ctr) /= CNT_DWORDS-1) then
                                en_len <= '1';
                            end if;
                        else
                            en_len <= '1';
                        end if;
                    else
                        --! AD or D segment
                        if (P_IS_BUFFER) then
                            if (A_EQ_D) then
                                if (unsigned(ctr) /= CNT_DWORDS-1) then
                                    en_len <= '1';
                                end if;
                            else
                                if ((sgmt_type(3 downto 2) = ST_A
                                        and unsigned(ctr) < CNT_AWORDS-1)
                                    or (sgmt_type(3 downto 2) /= ST_A
                                        and unsigned(ctr) /= CNT_DWORDS-1))
                                then
                                    en_len <= '1';
                                end if;
                            end if;
                        else
                            en_len <= '1';
                        end if;
                    end if;
                    if (P_IS_BUFFER) then
                        en_ctr  <= '1';
                        en_data <= '1';
                    end if;
                    en_ps <= '1';
                end if;

            when S_WAIT_READY =>
                pdi_rdy <= '0';
                ld_ctr  <= '1';
                if (unsigned(sgmt_len) = 0) then
                    if ((G_ENABLE_PAD and (G_PAD_AD > 2 or G_PAD_D > 2))
                        and is_extra = '1')
                    then
                        nps <= S_DATA;
                    else
                        if (sgmt_lst = '1') then
                            nps <= S_WAIT_INSTR;
                        else
                            nps <= S_WAIT_HDR;
                        end if;
                    end if;
                else
                    nps     <= S_DATA;
                end if;
                if (bdi_ready = '1') then
                    en_len  <= '1';
                    en_ps   <= '1';
                end if;
        end case;
    end process;

    sel_end <= '1' when (unsigned(sgmt_len) <= WB
                    and (is_extra = '0' and set_extra = '0'))
                else '0';

    gSdiComb:
    process(ss, s_instr_opcode, sdi_valid, ctr2, key_ready, slen)
    begin
        nss         <= ss;
        sdi_rdy     <= '0';
        en_key      <= '0';
        ld_ctr2     <= '0';
        ld_slen     <= '0';
        en_ctr2     <= '0';
        en_slen     <= '0';
        en_ss       <= '0';

        case ss is
            when S_WAIT_INSTR =>
                ld_ctr2     <= '1';
                sdi_rdy     <= '1';
                if (s_instr_opcode = OP_LDKEY) then
                    nss     <= S_WAIT_HDR;
                end if;
                if (sdi_valid = '1') then
                    en_ss <= '1';
                end if;

            when S_WAIT_HDR =>
                nss <= S_DATA;
                ld_slen     <= '1';
                sdi_rdy     <= '1';
                if (sdi_valid = '1') then
                    en_ss <= '1';
                end if;

            when S_DATA =>
                if (not S_IS_BUFFER) then
                    nss <= S_WAIT_INSTR;
                    if (sdi_valid = '1' and key_ready = '1') then
                        en_slen <= '1';
                        sdi_rdy <= '1';
                        if (unsigned(slen) = G_KEY_SIZE/8) then
                            en_ss   <= '1';
                        end if;
                    end if;
                else
                    sdi_rdy <= '1';
                    nss     <= S_WAIT_READY;
                    if (sdi_valid = '1') then
                        en_ctr2 <= '1';
                        en_key  <= '1';
                        if (unsigned(ctr2) = CNT_KWORDS-1)  then
                            en_ss   <= '1';
                        end if;
                    end if;
                end if;

            when S_WAIT_READY =>
                if (unsigned(slen) = G_KEY_SIZE/8) then
                    nss <= S_WAIT_INSTR;
                else
                    nss <= S_DATA;
                end if;
                ld_ctr2 <= '1';
                if (key_ready = '1') then
                    en_ss   <= '1';
                    en_slen <= '1';
                end if;

        end case;
    end process;
end architecture structure;
