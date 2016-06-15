-------------------------------------------------------------------------------
--! @file       CipherCore_Datapath.vhd
--! @author     Ekawat (ice) Homsirikamol
--! @brief      Datapath unit for ASCON
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity CipherCore_Datapath is
    port    (
        clk             : in  std_logic;
        rst             : in  std_logic;

        --! Input
        bdi             : in  std_logic_vector(64               -1 downto 0);
        bdi_valid_bytes : in  std_logic_vector( 8               -1 downto 0);
        key             : in  std_logic_vector(32               -1 downto 0);

        --! Datapath
        en_key          : in  std_logic;
        en_npub         : in  std_logic;
        en_state        : in  std_logic;
        en_cmp          : in  std_logic;
        is_last_ad      : in  std_logic;
        clr_rc          : in  std_logic;
        en_rc           : in  std_logic;
        sel_key_hi      : in  std_logic;
        sel_key_lo      : in  std_logic;
        sel_decrypt     : in  std_logic;
        sel_state       : in  std_logic_vector(2                -1 downto 0);
        sel_tag         : in  std_logic_vector(2                -1 downto 0);

        --! Output
        bdo             : out std_logic_vector(64               -1 downto 0);
        tag             : out std_logic_vector(128              -1 downto 0);
        msg_auth_valid  : out std_logic
    );
end entity CipherCore_Datapath;

architecture structure of CipherCore_Datapath is
    signal reg_key        : std_logic_vector(128                -1 downto 0);
    signal reg_npub_hi    : std_logic_vector(64                 -1 downto 0);
    signal bdi_xor        : std_logic_vector(64                 -1 downto 0);
    signal bdi_xor_sel    : std_logic_vector(64                 -1 downto 0);
    signal bdi_valid_bits : std_logic_vector(64                 -1 downto 0);
    signal init_state0    : std_logic_vector(320                -1 downto 0);
    signal init_statex    : std_logic_vector(320                -1 downto 0);
    signal istate         : std_logic_vector(320                -1 downto 0);
    signal ostate         : std_logic_vector(320                -1 downto 0);
    signal oround         : std_logic_vector(320                -1 downto 0);
    signal bdo_s          : std_logic_vector(64                 -1 downto 0);

    signal msg_auth_valid_s : std_logic;
    signal msg_auth_valid_r : std_logic;
    constant ZEROS        : std_logic_vector(128                -1 downto 0)
        := (others => '0');

    signal rc             : std_logic_vector(  8                -1 downto 0);
    signal rc_new         : std_logic_vector(  8                -1 downto 0);
begin
    p_clk:
    process(clk)
    begin
        if rising_edge(clk) then
            if (en_key = '1') then
                reg_key <= reg_key(95 downto 0) & key;
            end if;

            if (en_state = '1') then
                ostate  <= istate;
            end if;

            if (en_npub = '1') then
                reg_npub_hi <= bdi;
            end if;

            if (clr_rc = '1') then
                rc <= x"F0";
            elsif (en_rc = '1') then
                rc <= rc_new;
            end if;
            
            if (en_cmp = '1') then
                msg_auth_valid_r <= msg_auth_valid_s;
            end if;
        end if;
    end process;
    rc_new(7 downto 4) <= std_logic_vector(unsigned(rc(7 downto 4)) - "0001");
    rc_new(3 downto 0) <= std_logic_vector(unsigned(rc(3 downto 0)) + "0001");

    init_state0  <= x"80" & x"0C" & x"06" & ZEROS(39 downto 0)
                    & reg_key & reg_npub_hi & bdi;
    init_statex(319 downto 256) <=  bdi_xor_sel;
    init_statex(255 downto 128) <=  ostate(255 downto 128)                 when sel_key_hi = '0'  else (ostate(255 downto 128) xor reg_key);
    init_statex(127 downto   1) <=  ostate(127 downto   1)                 when sel_key_lo = '0'  else (ostate(127 downto   1) xor reg_key(127 downto   1));
    init_statex(             0) <= (ostate(             0) xor is_last_ad) when sel_key_lo = '0'  else (ostate(             0) xor reg_key(             0)  xor is_last_ad);

    gBits:
    for i in 7 downto 0  generate
        bdi_xor_sel(7+i*8 downto i*8) <= bdi(7+i*8 downto i*8)
            when (sel_decrypt = '1' and bdi_valid_bytes(i) = '1')
            else bdi_xor(7+i*8 downto i*8);
    end generate;

    with sel_state(1 downto 0) select
    istate <=   oround      when "00",
                init_statex when "01",
                init_state0 when others;

    u_round:
    entity work.ASCON_Round(structure)
    port map (ii => ostate, oo => oround, rc => rc);

    bdi_xor <= ostate(319 downto 256) xor bdi;

    with sel_tag select
    bdo_s <= init_statex(127 downto 64) when "10",
            init_statex(63 downto 0)    when "11",
            bdi_xor                     when others;
            
    bdo <= bdo_s;

    msg_auth_valid_s <= '1' when bdo_s = bdi else '0';
    msg_auth_valid <= msg_auth_valid_r and msg_auth_valid_s;
end architecture structure;