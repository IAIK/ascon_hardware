-------------------------------------------------------------------------------
--! @file       CipherCore.vhd
--! @brief      Cipher core for dummy1
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
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.ALL;
use ieee.numeric_std.ALL;
use work.AEAD_pkg.ALL;

entity CipherCore is
    generic (
        --! Reset behavior
        G_ASYNC_RSTN    : boolean := False; --! Async active low reset
        --! Block size (bits)
        G_DBLK_SIZE     : integer := 128;   --! Data
        G_KEY_SIZE      : integer := 128;   --! Key
        G_TAG_SIZE      : integer := 128;   --! Tag
        --! The number of bits required to hold block size expressed in
        --! bytes = log2_ceil(G_DBLK_SIZE/8)
        G_LBS_BYTES     : integer := 4;
        --! Algorithm parameter to simulate run-time behavior
        --! Warning: Do not set any number higher than 32
        G_LAT_KEYINIT   : integer := 4;     --! Key inialization latency
        G_LAT_PREP      : integer := 12;    --! Pre-processing latency (init)
        G_LAT_PROC_AD   : integer := 10;    --! Processing latency (per block)
        G_LAT_PROC_DATA : integer := 16;    --! Processing latency (per block)
        G_LAT_POST      : integer := 20     --! Post-processing latency (tag)
    );
    port (
        --! Global
        clk             : in  std_logic;
        rst             : in  std_logic;
        --! PreProcessor (data)
        key             : in  std_logic_vector(G_KEY_SIZE       -1 downto 0);
        bdi             : in  std_logic_vector(G_DBLK_SIZE      -1 downto 0);
        --! PreProcessor (controls)
        key_ready       : out std_logic;
        key_valid       : in  std_logic;
        key_update      : in  std_logic;
        decrypt         : in  std_logic;
        bdi_ready       : out std_logic;
        bdi_valid       : in  std_logic;
        bdi_type        : in  std_logic_vector(3                -1 downto 0);
        bdi_partial     : in  std_logic;
        bdi_eot         : in  std_logic;
        bdi_eoi         : in  std_logic;
        bdi_size        : in  std_logic_vector(G_LBS_BYTES+1    -1 downto 0);
        bdi_valid_bytes : in  std_logic_vector(G_DBLK_SIZE/8    -1 downto 0);
        bdi_pad_loc     : in  std_logic_vector(G_DBLK_SIZE/8    -1 downto 0);
        --! PostProcessor
        bdo             : out std_logic_vector(G_DBLK_SIZE      -1 downto 0);
        bdo_valid       : out std_logic;
        bdo_ready       : in  std_logic;
        bdo_size        : out std_logic_vector(G_LBS_BYTES+1    -1 downto 0);
        msg_auth_done   : out std_logic;
        msg_auth_valid  : out std_logic
    );
end entity CipherCore;

architecture structure of CipherCore is
    --! Registers
    signal bdi_r        : std_logic_vector(128      -1 downto 0);
    signal vbytes_r     : std_logic_vector(128/8    -1 downto 0);
    signal padloc_r     : std_logic_vector(128/8    -1 downto 0);
    signal key_block    : std_logic_vector(128      -1 downto 0);
    signal init_block   : std_logic_vector(128      -1 downto 0);
    signal ctr_block    : std_logic_vector(32       -1 downto 0);
    signal accum        : std_logic_vector(128      -1 downto 0);
    signal tag          : std_logic_vector(128      -1 downto 0);

    --! Signals
    signal ctr_block128 : std_logic_vector(128      -1 downto 0);
    signal accum_or_ctr : std_logic_vector(128      -1 downto 0);
    signal to_output    : std_logic_vector(128      -1 downto 0);
    signal ad_or_msg    : std_logic_vector(128      -1 downto 0);

    --! Controls
    --!     Register
    signal is_decrypt   : std_logic;
    --!     Combinatorial
    signal ctr          : std_logic_vector(5        -1 downto 0);
    signal clr_accum    : std_logic;
    signal ld_accum     : std_logic;
    signal ld_key       : std_logic;
    signal ld_npub      : std_logic;
    signal en_block     : std_logic;
    signal ld_ctr       : std_logic;
    signal ld_input     : std_logic;
    signal en_ctr       : std_logic;
    signal en_tag       : std_logic;
    signal sel_decrypt  : std_logic;
    signal sel_final    : std_logic;

    type t_state is (S_INIT, S_WAIT_START, S_WAIT_KEY, S_INIT_KEY,
        S_WAIT_NPUB, S_INIT_MSG, S_WAIT_MSG,
        S_PROC_AD, S_PROC_DATA, S_PROC_LENGTH,
        S_WAIT_TAG_AUTH);
    signal state        : t_state;
    signal nstate       : t_state;

begin
    --! =======================================================================
    --! Datapath
    --! =======================================================================
    process(clk)
    begin
        if rising_edge(clk) then
            if (ld_input = '1') then
                bdi_r     <= bdi;
                vbytes_r  <= bdi_valid_bytes;
                padloc_r  <= bdi_pad_loc;
            end if;

            if (ld_key = '1') then
                key_block <= key;
            end if;
            if (ld_npub = '1') then
                init_block <= bdi xor key_block;
            end if;

            if (clr_accum = '1') then
                accum     <= (others => '0');
            elsif (ld_accum = '1') then
                accum     <= ad_or_msg xor accum;
            end if;

            if (clr_accum = '1') then
                ctr_block <= (0 => '1', others => '0');
            elsif (en_block = '1') then
                ctr_block <= std_logic_vector(unsigned(ctr_block) + 1);
            end if;

            if (en_tag = '1') then
                tag <= to_output;
            end if;
        end if;
    end process;

    ctr_block128(127 downto 32) <= (others => '0');
    ctr_block128(31  downto 0 ) <= ctr_block;
    accum_or_ctr <= accum when sel_final = '1' else ctr_block128;
    to_output <= bdi_r xor accum_or_ctr xor init_block;
    


    aBlock: block
        signal vbits : std_logic_vector(G_DBLK_SIZE-1 downto 0);
        signal data  : std_logic_vector(G_DBLK_SIZE-1 downto 0);
        signal pad   : std_logic_vector(G_DBLK_SIZE-1 downto 0);
        signal ext   : std_logic_vector(G_DBLK_SIZE-1 downto 0);
    begin
        
        gVbits:
        for i in 0 to G_DBLK_SIZE/8-1 generate
            --! Get valid bits from valid bytes
            vbits(8*i+7 downto 8*i) <= (others => vbytes_r(i));           
            --! Get padding bit 
            pad  (8*i+7           ) <= padloc_r(i);
            pad  (8*i+6 downto 8*i) <= (others => '0');
        end generate;
        
        
        
        --! Use sel_final signal as a flag to determine whether
        --! vbits should be used or not.
        --! Note: sel_final is tag
        ext(127 downto 0) <= (others => sel_final);
        data <= bdi_r when sel_decrypt = '0' else to_output;
        ad_or_msg <= (data and vbits) xor pad;
        bdo <= to_output and (vbits or ext);
    end block;

    msg_auth_valid <= '1' when tag = bdi else '0';

    --! =======================================================================
    --! Control
    --! =======================================================================
    gSyncRst:
    if (not G_ASYNC_RSTN) generate
        process(clk)
        begin
            if rising_edge(clk) then
                if (rst = '1') then
                    state <= S_INIT;
                else
                    state <= nstate;
                end if;
            end if;
        end process;
    end generate;
    gAsyncRstn:
    if (G_ASYNC_RSTN) generate
        process(clk, rst)
        begin
            if (rst = '0') then
                state <= S_INIT;
            elsif rising_edge(clk) then
                state <= nstate;
            end if;
        end process;
    end generate;

    process(clk)
    begin
        if rising_edge(clk) then
            if (ld_ctr = '1') then
                ctr <= (others => '0');
            elsif (en_ctr = '1') then
                ctr <= std_logic_vector(unsigned(ctr) + 1);
            end if;
            --! Store Decrypt signal internally for use during the tag
            --! authentication state.
            if (state = S_WAIT_START) then
                is_decrypt <= decrypt;
            end if;
        end if;
    end process;

    process(
        state, key_valid, key_update, is_decrypt, ctr,
        bdi_valid, bdi_type, bdi_eot, bdi_eoi, bdi_size,
        bdo_ready)
    begin
        --! Internal
        nstate      <= state;
        clr_accum   <= '0';
        ld_accum    <= '0';
        ld_key      <= '0';
        ld_npub     <= '0';
        en_block    <= '0';
        ld_ctr      <= '0';
        ld_input    <= '0';
        en_tag      <= '0';
        en_ctr      <= '0';
        sel_decrypt <= '0';
        sel_final   <= '0';
        --! External
        key_ready   <= '0';
        bdi_ready   <= '0';
        bdo_valid   <= '0';
        msg_auth_done <= '0';

        case state is
            when S_INIT =>
                --! After reset
                clr_accum <= '1';
                ld_ctr    <= '1';
                nstate <= S_WAIT_START;

            when S_WAIT_START =>
                --! Needs to check whether a new input is available first
                --! prior to checking key to ensure the correct operational
                --! step.
                if (bdi_valid = '1') then
                    if (key_update = '1') then
                        nstate <= S_WAIT_KEY;
                    else
                        nstate <= S_WAIT_NPUB;
                    end if;
                end if;

            when S_WAIT_KEY =>
                --! Wait for key
                if (key_valid = '1') then
                    key_ready <= '1';
                    ld_key    <= '1';
                    nstate    <= S_INIT_KEY;
                end if;

            when S_INIT_KEY =>
                --! Simulate key initialization delay
                en_ctr <= '1';
                if (unsigned(ctr) = G_LAT_KEYINIT-1) then
                    ld_ctr <= '1';
                    nstate <= S_WAIT_NPUB;
                end if;

            when S_WAIT_NPUB =>
                bdi_ready <= '1';
                if (bdi_valid = '1') then
                    ld_npub <= '1';
                    nstate  <= S_INIT_MSG;
                end if;

            when S_INIT_MSG =>
                --! Simulate msg initialization delay
                en_ctr <= '1';
                if (unsigned(ctr) = G_LAT_PREP-1) then
                    ld_ctr <= '1';
                    nstate <= S_WAIT_MSG;
                end if;

            when S_WAIT_MSG =>
                --! Accumulate AD onto accum register
                bdi_ready <= '1';
                if (bdi_valid = '1') then
                    ld_input  <= '1';
                    if (bdi_type = BDI_TYPE_ASS0) then
                        --! Note: Assume ST_AD is used (see: AEAD_pkg.vhd)
                        nstate <= S_PROC_AD;
                    elsif (bdi_type = BDI_TYPE_DAT0) then
                        nstate <= S_PROC_DATA;
                    else
                        --! Length type
                        nstate <= S_PROC_LENGTH;
                    end if;
                end if;

            when S_PROC_AD =>
                --! Process AD
                en_ctr <= '1';
                if (unsigned(ctr) = G_LAT_PROC_AD-1) then
                    ld_accum <= '1';
                    ld_ctr   <= '1';
                    nstate   <= S_WAIT_MSG;
                end if;

            when S_PROC_DATA =>
                --! Process Data
                if (unsigned(ctr) = G_LAT_PROC_DATA-1) then
                    if (bdo_ready = '1') then
                        sel_decrypt <= is_decrypt;
                        ld_accum    <= '1';
                        bdo_valid   <= '1';
                        ld_ctr      <= '1';
                        en_block    <= '1';    
                        nstate      <= S_WAIT_MSG;
                    end if;
                else
                    en_ctr <= '1';
                end if;

            when S_PROC_LENGTH =>
                --! Process Length (generate tag)
                if (unsigned(ctr) = G_LAT_POST-1) then
                    if (bdo_ready = '1' or is_decrypt = '1') then
                        sel_final <= '1';
                        ld_ctr    <= '1';
                        if (is_decrypt = '1') then
                            en_tag    <= '1';
                            nstate    <= S_WAIT_TAG_AUTH;
                        else
                            bdo_valid <= '1';
                            nstate    <= S_INIT;
                        end if;
                    end if;
                else
                    en_ctr <= '1';
                end if;

            when S_WAIT_TAG_AUTH =>
                --! Wait to compare tag
                bdi_ready <= '1';
                if (bdi_valid = '1') then                    
                    msg_auth_done <= '1';
                    nstate        <= S_INIT;
                end if;
        end case;

    end process;
end structure;