-------------------------------------------------------------------------------
--! @file       CipherCore_Control.vhd
--! @author     Ekawat (ice) Homsirikamol
--! @brief      Control unit for ASCON
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity CipherCore_Control is
    port (
        clk             : in  std_logic;
        rst             : in  std_logic;

        --! Input
        bdi             : in  std_logic_vector(64               -1 downto 0);
        key_ready       : out std_logic;
        key_valid       : in  std_logic;
        key_update      : in  std_logic;
        decrypt         : in  std_logic;
        bdi_ready       : out std_logic;
        bdi_valid       : in  std_logic;
        bdi_type        : in  std_logic_vector(3                -1 downto 0);
        bdi_eot         : in  std_logic;
        bdi_eoi         : in  std_logic;
        bdi_size        : in  std_logic_vector(4                -1 downto 0);

        --! Datapath
        en_key          : out std_logic;
        en_state        : out std_logic;
        en_npub         : out std_logic;
        en_cmp          : out std_logic;
        is_last_ad      : out std_logic;
        clr_rc          : out std_logic;
        en_rc           : out std_logic;
        sel_key_hi      : out std_logic;
        sel_key_lo      : out std_logic;
        sel_decrypt     : out std_logic;
        sel_state       : out std_logic_vector(2               -1 downto 0);
        sel_tag         : out std_logic_vector(2               -1 downto 0);

        --! Output
        msg_auth_done   : out std_logic;
        bdo_ready       : in  std_logic;
        bdo_valid       : out std_logic
    );
end entity CipherCore_Control;

architecture behavior of CipherCore_Control is
    type state_type is (S_WAIT_KEY, S_LD_KEY, S_LD_LEN,
                        S_LD_NPUB0, S_LD_NPUB1,
                        S_PROCESS, S_WAIT, S_WAIT_OUT_TAG2);
    signal state          : state_type;
    signal nstate         : state_type;

    constant TOT_ROUND_HI : integer := 12;
    constant TOT_ROUND_LO : integer := 6;

    signal set_compute_hi : std_logic;
    signal set_compute_lo : std_logic;
    signal clr_last_ad    : std_logic;
    signal set_last_ad    : std_logic;
    signal clr_round      : std_logic;
    signal en_round       : std_logic;
    signal clr_tag        : std_logic;
    signal set_tag        : std_logic;
    signal is_tag         : std_logic;
    signal is_decrypt  : std_logic;

    signal rndcmp   : std_logic_vector( 4           -1 downto 0);
    signal round    : std_logic_vector( 4           -1 downto 0);
begin
    p_reg:
    process( clk )
    begin
        if rising_edge( clk ) then
            if rst = '1' then
                is_last_ad <= '0';
                round      <= (others => '0');
                state      <= S_WAIT_KEY;
                is_tag     <= '0';
                is_last_ad <= '0';
                sel_key_lo <= '0';
            else
                state   <= nstate;
                if (clr_round = '1') then
                    round <= (others => '0');
                elsif (en_round = '1') then
                    round <= std_logic_vector(unsigned(round) + 1);
                end if;

                if (clr_last_ad = '1') then
                    is_last_ad <= '0';
                elsif (set_last_ad = '1') then
                    is_last_ad <= '1';
                end if;

                if (set_compute_hi = '1') then
                    rndcmp <= std_logic_vector(to_unsigned((TOT_ROUND_HI-1), 4));
                elsif (set_compute_lo = '1') then
                    rndcmp <= std_logic_vector(to_unsigned((TOT_ROUND_LO-1), 4));
                end if;

                if (clr_tag = '1' or set_tag = '1') then
                    sel_key_lo <= '1';
                elsif (set_compute_lo = '1') then
                    sel_key_lo <= '0';
                end if;

                if (clr_tag = '1') then
                    is_tag <= '0';
                elsif (set_tag = '1') then
                    is_tag <= '1';
                end if;

                if (state = S_LD_LEN) then
                    is_decrypt <= decrypt;
                end if;
            end if;
        end if;
    end process;
    clr_rc      <= clr_round;
    en_rc       <= en_round;
    sel_key_hi  <= set_tag;
    sel_tag     <= is_tag & round(0);    

    p_state:
    process( state, bdi_valid, bdi_type, is_decrypt,
             bdi_eot, bdo_ready, bdi, bdi_size,
             key_valid, key_update, round, rndcmp, is_tag
             )
    begin
        --! External
        key_ready      <= '0';
        bdi_ready      <= '0';
        bdo_valid      <= '0';
        msg_auth_done  <= '0';

        --! Datapath
        en_key         <= '0';
        en_state       <= '0';
        en_npub        <= '0';
        en_cmp         <= '0';
        set_compute_hi <= '0';
        set_compute_lo <= '0';
        sel_decrypt    <= '0';
        sel_state      <= "00";
        set_tag        <= '0';

        --! Internal
        clr_round      <= '0';
        clr_tag        <= '0';
        clr_last_ad    <= '0';
        set_last_ad    <= '0';
        en_round       <= '0';
        nstate         <= state;

        case state is
            when S_WAIT_KEY =>
                clr_round   <= '1';
                clr_tag     <= '1';
                set_compute_hi  <= '1';
                if (key_update = '1' or bdi_valid = '1') then
                    if (key_update = '1') then
                        nstate <= S_LD_KEY;
                    else
                        nstate <= S_LD_LEN;
                    end if;
                end if;

            when S_LD_KEY =>
                key_ready <= '1';
                if (key_valid = '1') then
                    en_key   <= '1';
                    en_round <= '1';
                    if (unsigned(round) = 3) then
                        nstate    <= S_LD_LEN;
                        clr_round <= '1';
                    end if;
                end if;

            when S_LD_LEN =>
                --! Determine if AD is empty
                if (unsigned(bdi(63 downto 32)) = 0) then
                    set_last_ad <= '1';
                else
                    clr_last_ad <= '1';
                end if;
                bdi_ready <= '1';
                if (bdi_valid = '1') then
                    nstate <= S_LD_NPUB0;
                end if;

            when S_LD_NPUB0 =>
                --! Store the first Npub block in the register
                bdi_ready <= '1';
                en_npub   <= '1';
                if (bdi_valid = '1') then
                    nstate <= S_LD_NPUB1;
                end if;

            when S_LD_NPUB1 =>
                --! Get the second Npub and start processing
                bdi_ready <= '1';
                en_state  <= '1';
                sel_state <= "10";
                if (bdi_valid = '1') then
                    nstate    <= S_PROCESS;
                end if;

            when S_PROCESS =>
                --! Process
                en_round <= '1';
                en_state <= '1';
                if (round = rndcmp) then
                    clr_round <= '1';
                    nstate    <= S_WAIT;
                end if;

            when S_WAIT =>
                --! Load/Output data
                sel_state       <= "01";
                if (is_tag = '1') then
                    if (is_decrypt = '0' and bdo_ready = '1') then
                        bdo_valid     <= '1';
                        en_round      <= '1';
                        nstate        <= S_WAIT_OUT_TAG2;
                    elsif (is_decrypt = '1' and bdi_valid = '1') then
                        bdi_ready     <= '1';
                        en_round      <= '1';
                        en_cmp        <= '1';
                        nstate        <= S_WAIT_OUT_TAG2;
                    end if;
                else
                    if (bdi_valid = '1'  
                        and (bdi_type(2 downto 1) = "00"
                            or bdo_ready = '1'
                            or not unsigned(bdi_size) /= 0)) 
                    then
                        bdi_ready  <= '1';
                        en_state  <= '1';
                        if (bdi_type(2 downto 1) = "00") then
                            set_compute_lo <= '1';
                            if (bdi_eot = '1') then
                                set_last_ad <= '1';
                            end if;
                        else
                            if (unsigned(bdi_size) /= 0) then
                                bdo_valid   <= '1';
                            end if;
                            clr_last_ad <= '1';
                            if (is_decrypt = '1'
                                and unsigned(bdi_size) /= 0)
                            then
                                sel_decrypt <= '1';
                            end if;
                            if (bdi_eot = '1') then
                                set_compute_hi <= '1';
                                set_tag        <= '1';
                            else
                                set_compute_lo <= '1';
                            end if;
                        end if;
                        nstate <= S_PROCESS;
                    end if;
                end if;

            when S_WAIT_OUT_TAG2 =>
                if (is_decrypt = '0') then
                    bdo_valid <= '1';
                    if (bdo_ready = '1') then
                        nstate <= S_WAIT_KEY;
                    end if;
                else
                    bdi_ready <= '1';
                    if (bdi_valid = '1') then
                        msg_auth_done <= '1';
                        nstate <= S_WAIT_KEY;
                    end if;
                end if;

        end case;
    end process;

end behavior;
