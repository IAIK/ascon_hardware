-------------------------------------------------------------------------------
-- Title      : A naturally fast implementation of Ascon
-- Project    : 
-------------------------------------------------------------------------------
-- File       : ascon_ref.vhdl
-- Author     : Erich Wenger  <erich.wenger@iaik.tugraz.at>
-- Company    : 
-- Created    : 2014-03-21
-- Last update: 2014-05-23
-- Platform   : 
-- Standard   : VHDL'93/02
-------------------------------------------------------------------------------
-- Description: In a 90nm UMC technology, this design is 8708 GE large and
-- needs 7/8=0.875 cycles to encrypt a byte (assuming a large message).
-------------------------------------------------------------------------------
-- Copyright (c) 2014
-- This work is licensed under the Creative Commons
-- Attribution-NonCommercial-ShareAlike 4.0 International License.
-- To view a copy of this license, visit http://creativecommons.org/licenses/by-nc-sa/4.0/.
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author           Description
-- 2014-03-21  1.0      Erich Wenger     Created
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity ascon is
  
  generic (
    KEY_SIZE        : integer := 128;
    DATA_BLOCK_SIZE : integer := 64;
    ROUNDS_A        : integer := 12;
    ROUNDS_B        : integer := 6;
    DATA_BUS_WIDTH  : integer := 32;
    ADDR_BUS_WIDTH  : integer := 8);

  port (
    ClkxCI       : in  std_logic;
    RstxRBI      : in  std_logic;
    CSxSI        : in  std_logic;       -- active-high chip select
    WExSI        : in  std_logic;       -- active-high write enable
    AddressxDI   : in  std_logic_vector(ADDR_BUS_WIDTH-1 downto 0);
    DataWritexDI : in  std_logic_vector(DATA_BUS_WIDTH-1 downto 0);
    DataReadxDO  : out std_logic_vector(DATA_BUS_WIDTH-1 downto 0));

end entity ascon;

architecture structural of ascon is

  constant CONTROL_STATE_SIZE : integer := 4;
  constant STATE_WORD_SIZE    : integer := 64;

  constant CONST_KEY_SIZE : std_logic_vector(7 downto 0) := std_logic_vector(to_unsigned(KEY_SIZE, 8));
  constant CONST_ROUNDS_A : std_logic_vector(7 downto 0) := std_logic_vector(to_unsigned(ROUNDS_A, 8));
  constant CONST_ROUNDS_B : std_logic_vector(7 downto 0) := std_logic_vector(to_unsigned(ROUNDS_B, 8));

  signal KeyxDP, KeyxDN                   : std_logic_vector(KEY_SIZE-1 downto 0);
  signal IODataxDP, IODataxDN             : std_logic_vector(DATA_BLOCK_SIZE-1 downto 0);
  signal State0xDP, State0xDN             : std_logic_vector(STATE_WORD_SIZE-1 downto 0);
  signal State1xDP, State1xDN             : std_logic_vector(STATE_WORD_SIZE-1 downto 0);
  signal State2xDP, State2xDN             : std_logic_vector(STATE_WORD_SIZE-1 downto 0);
  signal State3xDP, State3xDN             : std_logic_vector(STATE_WORD_SIZE-1 downto 0);
  signal State4xDP, State4xDN             : std_logic_vector(STATE_WORD_SIZE-1 downto 0);
  signal ControlStatexDP, ControlStatexDN : std_logic_vector(CONTROL_STATE_SIZE-1 downto 0);

  signal StatexDP : std_logic_vector(5*STATE_WORD_SIZE-1 downto 0);

  signal DP_WriteNoncexS  : std_logic;
  signal DP_WriteIODataxS : std_logic;

  signal CP_InitxSP, CP_InitxSN                               : std_logic;
  signal CP_AssociatexSP, CP_AssociatexSN                     : std_logic;
  signal CP_EncryptxSP, CP_EncryptxSN                         : std_logic;
  signal CP_DecryptxSP, CP_DecryptxSN                         : std_logic;
  signal CP_FinalAssociatedDataxSP, CP_FinalAssociatedDataxSN : std_logic;
  signal CP_FinalEncryptxSP, CP_FinalEncryptxSN               : std_logic;
  signal CP_FinalDecryptxSP, CP_FinalDecryptxSN               : std_logic;
  signal CP_DonexS                                            : std_logic;

  signal DP_InitxS      : std_logic;
  signal DP_RoundxS     : std_logic;
  signal DP_XorZKeyxS   : std_logic;
  signal DP_XorKeyZxS   : std_logic;
  signal DP_XorZOnexS   : std_logic;
  signal DP_EncryptxS   : std_logic;
  signal DP_DecryptxS   : std_logic;
  signal DP_AssociatexS : std_logic;

  function ZEROS (
    constant WIDTH : natural)
    return std_logic_vector is
    variable x : std_logic_vector(WIDTH-1 downto 0);
  begin  -- ZEROS
    x := (others => '0');
    return x;
  end ZEROS;

  function ROTATE_STATE_WORD (
    word            : std_logic_vector(STATE_WORD_SIZE-1 downto 0);
    constant rotate : integer)
    return std_logic_vector is
    variable x : std_logic_vector(STATE_WORD_SIZE-1 downto 0);
  begin  -- ROTATE_STATE_WORD
    x := word(ROTATE-1 downto 0) & word(STATE_WORD_SIZE-1 downto ROTATE);
    return x;
  end ROTATE_STATE_WORD;
  
begin  -- architecture structural

  StatexDP <= State4xDP & State3xDP & State2xDP & State1xDP & State0xDP;

  -- purpose: Defines all registers
  -- type   : sequential
  -- inputs : ClkxCI, RstxRBI, *xDN signals
  -- outputs: *xDP signals
  RegisterProc : process (ClkxCI, RstxRBI) is
  begin  -- process RegisterProc
    if RstxRBI = '0' then               -- asynchronous reset (active low)
      KeyxDP                    <= (others => '0');
      IODataxDP                 <= (others => '0');
      State0xDP                 <= (others => '0');
      State1xDP                 <= (others => '0');
      State2xDP                 <= (others => '0');
      State3xDP                 <= (others => '0');
      State4xDP                 <= (others => '0');
      ControlStatexDP           <= (others => '0');
      CP_InitxSP                <= '0';
      CP_AssociatexSP           <= '0';
      CP_EncryptxSP             <= '0';
      CP_DecryptxSP             <= '0';
      CP_FinalAssociatedDataxSP <= '0';
      CP_FinalEncryptxSP        <= '0';
      CP_FinalDecryptxSP        <= '0';
    elsif ClkxCI'event and ClkxCI = '1' then  -- rising clock edge
      KeyxDP                    <= KeyxDN;
      IODataxDP                 <= IODataxDN;
      State0xDP                 <= State0xDN;
      State1xDP                 <= State1xDN;
      State2xDP                 <= State2xDN;
      State3xDP                 <= State3xDN;
      State4xDP                 <= State4xDN;
      ControlStatexDP           <= ControlStatexDN;
      CP_InitxSP                <= CP_InitxSN;
      CP_AssociatexSP           <= CP_AssociatexSN;
      CP_EncryptxSP             <= CP_EncryptxSN;
      CP_DecryptxSP             <= CP_DecryptxSN;
      CP_FinalAssociatedDataxSP <= CP_FinalAssociatedDataxSN;
      CP_FinalEncryptxSP        <= CP_FinalEncryptxSN;
      CP_FinalDecryptxSP        <= CP_FinalDecryptxSN;
    end if;
  end process RegisterProc;


  -- purpose: Glue the internal registers with the bus
  -- type   : combinational
  DataBusLogicProc : process (AddressxDI, CP_AssociatexSP, CP_DecryptxSP,
                              CP_DonexS, CP_EncryptxSP, CP_FinalDecryptxSP,
                              CP_FinalEncryptxSP, CP_InitxSP, CSxSI,
                              DataWritexDI, IODataxDP, KeyxDP, State3xDP,
                              State4xDP, StatexDP, WExSI) is
    variable AddressxDV : integer;
    variable index      : integer;
  begin  -- process DataBusLogicProc
    KeyxDN <= KeyxDP;

    AddressxDV  := to_integer(unsigned(AddressxDI));
    index       := 0;
    DataReadxDO <= (others => '0');

    DP_WriteNoncexS  <= '0';
    DP_WriteIODataxS <= '0';

    CP_InitxSN         <= CP_InitxSP;
    CP_AssociatexSN    <= CP_AssociatexSP;
    CP_EncryptxSN      <= CP_EncryptxSP;
    CP_DecryptxSN      <= CP_DecryptxSP;
    CP_FinalEncryptxSN <= CP_FinalEncryptxSP;
    CP_FinalDecryptxSN <= CP_FinalDecryptxSP;

    if CP_DonexS = '1' then
      CP_InitxSN         <= '0';
      CP_AssociatexSN    <= '0';
      CP_EncryptxSN      <= '0';
      CP_DecryptxSN      <= '0';
      CP_FinalEncryptxSN <= '0';
      CP_FinalDecryptxSN <= '0';
    end if;

    -- TODO: only designed for DATA_BUS_WIDTH=32
    if CSxSI = '1' then
      if WExSI = '1' then
        -- synchronous write
        if AddressxDV = 2 then
          -- command register
          CP_InitxSN         <= DataWritexDI(0);
          CP_AssociatexSN    <= DataWritexDI(1);
          CP_EncryptxSN      <= DataWritexDI(2);
          CP_DecryptxSN      <= DataWritexDI(3);
          CP_FinalEncryptxSN <= DataWritexDI(4);
          CP_FinalDecryptxSN <= DataWritexDI(5);
        elsif (AddressxDV >= 4) and (AddressxDV < 8) then
          -- write the key
          index                                                          := to_integer(unsigned(AddressxDI(1 downto 0)));
          KeyxDN((index+1)*DATA_BUS_WIDTH-1 downto index*DATA_BUS_WIDTH) <= DataWritexDI;
        elsif (AddressxDV >= 8) and (AddressxDV < 12) then
          -- write the nonce
          DP_WriteNoncexS <= '1';
        elsif (AddressxDV >= 12) and (AddressxDV < 14) then
          -- write the data to de/encrypt and associated data
          DP_WriteIODataxS <= '1';
        end if;
      else
        -- asynchronous read
        if AddressxDV = 0 then
          DataReadxDO <= x"deadbeef";
        elsif AddressxDV = 1 then
          -- status register
          -- returns 1 if busy
          DataReadxDO(0) <= CP_InitxSP or CP_AssociatexSP or CP_EncryptxSP or CP_DecryptxSP or CP_FinalEncryptxSP or CP_FinalDecryptxSP;
        elsif (AddressxDV >= 12) and (AddressxDV < 14) then
          -- read the de/encrypted data and associated data
          index       := to_integer(unsigned(AddressxDI(0 downto 0)));
          DataReadxDO <= IODataxDP((index+1)*DATA_BUS_WIDTH-1 downto index*DATA_BUS_WIDTH);
        elsif (AddressxDV >= 16) and (AddressxDV < 20) then
          -- read the tag
          if AddressxDI(1 downto 0) = "00" then
            DataReadxDO <= State4xDP(31 downto 0);
          elsif AddressxDI(1 downto 0) = "01" then
            DataReadxDO <= State4xDP(63 downto 32);
          elsif AddressxDI(1 downto 0) = "10" then
            DataReadxDO <= State3xDP(31 downto 0);
          else
            DataReadxDO <= State3xDP(63 downto 32);
          end if;
        end if;
      end if;
    end if;
  end process DataBusLogicProc;

  -- purpose: Controlpath of Ascon
  -- type   : combinational
  ControlProc : process (CP_AssociatexSP, CP_DecryptxSP, CP_EncryptxSP,
                         CP_FinalAssociatedDataxSP, CP_FinalDecryptxSP,
                         CP_FinalEncryptxSP, CP_InitxSP, ControlStatexDP) is
    variable ControlStatexDV : integer;
  begin  -- process ControlProc

    CP_FinalAssociatedDataxSN <= CP_FinalAssociatedDataxSP;

    DP_InitxS      <= '0';
    DP_RoundxS     <= '0';
    DP_XorZKeyxS   <= '0';
    DP_XorKeyZxS   <= '0';
    DP_XorZOnexS   <= '0';
    DP_EncryptxS   <= '0';
    DP_DecryptxS   <= '0';
    CP_DonexS      <= '0';
    DP_AssociatexS <= '0';

    ControlStatexDV := to_integer(unsigned(ControlStatexDP));
    ControlStatexDN <= ControlStatexDP;

    if ControlStatexDV = 0 then
      DP_InitxS      <= CP_InitxSP;
      DP_AssociatexS <= CP_AssociatexSP;
      DP_EncryptxS   <= CP_EncryptxSP or CP_FinalEncryptxSP;
      DP_DecryptxS   <= CP_DecryptxSP or CP_FinalDecryptxSP;
      DP_XorKeyZxS   <= CP_FinalEncryptxSP or CP_FinalDecryptxSP;

      if CP_InitxSP = '1' then
        CP_FinalAssociatedDataxSN <= '0';
      end if;
      if (CP_EncryptxSP = '1') or (CP_DecryptxSP = '1') or (CP_FinalEncryptxSP = '1') or (CP_FinalDecryptxSP = '1') then
        CP_FinalAssociatedDataxSN <= '1';
        if CP_FinalAssociatedDataxSP = '0' then
          DP_XorZOnexS <= '1';
        end if;
      end if;
    end if;

    if (CP_InitxSP = '1') or (CP_AssociatexSP = '1') or (CP_EncryptxSP = '1') or (CP_DecryptxSP = '1') or (CP_FinalEncryptxSP = '1') or (CP_FinalDecryptxSP = '1') then
      ControlStatexDN <= std_logic_vector(unsigned(ControlStatexDP) + 1);
      DP_RoundxS      <= '1';
    end if;

    if ((CP_InitxSP = '1') or (CP_FinalEncryptxSP = '1') or (CP_FinalDecryptxSP = '1')) and (ControlStatexDV = ROUNDS_A-1) then
      ControlStatexDN <= (others => '0');
      DP_XorZKeyxS    <= '1';
      CP_DonexS       <= '1';
    end if;

    if ((CP_AssociatexSP = '1') or (CP_EncryptxSP = '1') or (CP_DecryptxSP = '1')) and (ControlStatexDV = ROUNDS_B-1) then
      ControlStatexDN <= (others => '0');
      CP_DonexS       <= '1';
    end if;
  end process ControlProc;

  -- purpose: Datapath of Ascon
  -- type   : combinational
  DatapathProc : process (AddressxDI, ControlStatexDP, DP_AssociatexS,
                          DP_DecryptxS, DP_EncryptxS, DP_InitxS, DP_RoundxS,
                          DP_WriteIODataxS, DP_WriteNoncexS, DP_XorKeyZxS,
                          DP_XorZKeyxS, DP_XorZOnexS, DataWritexDI, IODataxDP,
                          KeyxDP, State0xDP, State1xDP, State2xDP, State3xDP,
                          State4xDP) is
    variable P0xDV, P1xDV, P2xDV, P3xDV, P4xDV : std_logic_vector(STATE_WORD_SIZE-1 downto 0);
    variable R0xDV, R1xDV, R2xDV, R3xDV, R4xDV : std_logic_vector(STATE_WORD_SIZE-1 downto 0);
    variable S0xDV, S1xDV, S2xDV, S3xDV, S4xDV : std_logic_vector(STATE_WORD_SIZE-1 downto 0);
    variable T0xDV, T1xDV, T2xDV, T3xDV, T4xDV : std_logic_vector(STATE_WORD_SIZE-1 downto 0);
    variable U0xDV, U1xDV, U2xDV, U3xDV, U4xDV : std_logic_vector(STATE_WORD_SIZE-1 downto 0);
    variable RoundConstxDV                     : std_logic_vector(63 downto 0);
    variable State0XorIODataxDV                : std_logic_vector(63 downto 0);
  begin  -- process DatapathProc

    RoundConstxDV      := ZEROS(64-8) & not ControlStatexDP(3 downto 0) & ControlStatexDP(3 downto 0);
    State0XorIODataxDV := State0xDP xor IODataxDP;

    IODataxDN <= IODataxDP;
    State0xDN <= State0xDP;
    State1xDN <= State1xDP;
    State2xDN <= State2xDP;
    State3xDN <= State3xDP;
    State4xDN <= State4xDP;

    P0xDV := State0xDP;
    P1xDV := State1xDP;
    P2xDV := State2xDP;
    P3xDV := State3xDP;
    P4xDV := State4xDP;

    if DP_InitxS = '1' then
      P0xDV := CONST_KEY_SIZE & CONST_ROUNDS_A & CONST_ROUNDS_B & ZEROS(64-3*8);
      P1xDV := KeyxDP(127 downto 64);
      P2xDV := KeyxDP(63 downto 0);
    -- State3xDP and State4xDP were already initialized with the NONCE
    end if;

    if DP_XorKeyZxS = '1' then
      P1xDV := State1xDP xor KeyxDP(127 downto 64);
      P2xDV := State2xDP xor KeyxDP(63 downto 0);
    end if;

    P4xDV(0) := P4xDV(0) xor DP_XorZOnexS;

    if (DP_EncryptxS = '1') or (DP_AssociatexS = '1') then
      P0xDV := State0XorIODataxDV;
    end if;
    if (DP_EncryptxS = '1') then
      IODataxDN <= P0xDV;
    end if;

    if DP_DecryptxS = '1' then
      P0xDV     := IODataxDP;
      IODataxDN <= State0XorIODataxDV;
    end if;

    R0xDV := P0xDV xor P4xDV;
    R1xDV := P1xDV;
    R2xDV := P2xDV xor P1xDV xor RoundConstxDV;
    R3xDV := P3xDV;
    R4xDV := P4xDV xor P3xDV;

    S0xDV := R0xDV xor (not R1xDV and R2xDV);
    S1xDV := R1xDV xor (not R2xDV and R3xDV);
    S2xDV := R2xDV xor (not R3xDV and R4xDV);
    S3xDV := R3xDV xor (not R4xDV and R0xDV);
    S4xDV := R4xDV xor (not R0xDV and R1xDV);

    T0xDV := S0xDV xor S4xDV;
    T1xDV := S1xDV xor S0xDV;
    T2xDV := not S2xDV;
    T3xDV := S3xDV xor S2xDV;
    T4xDV := S4xDV;

    U0xDV := T0xDV xor ROTATE_STATE_WORD(T0xDV, 19) xor ROTATE_STATE_WORD(T0xDV, 28);
    U1xDV := T1xDV xor ROTATE_STATE_WORD(T1xDV, 61) xor ROTATE_STATE_WORD(T1xDV, 39);
    U2xDV := T2xDV xor ROTATE_STATE_WORD(T2xDV, 1) xor ROTATE_STATE_WORD(T2xDV, 6);
    U3xDV := T3xDV xor ROTATE_STATE_WORD(T3xDV, 10) xor ROTATE_STATE_WORD(T3xDV, 17);
    U4xDV := T4xDV xor ROTATE_STATE_WORD(T4xDV, 7) xor ROTATE_STATE_WORD(T4xDV, 41);

    if DP_XorZKeyxS = '1' then
      U3xDV := U3xDV xor KeyxDP(127 downto 64);
      U4xDV := U4xDV xor KeyxDP(63 downto 0);
    end if;

    if DP_RoundxS = '1' then
      State0xDN <= U0xDV;
      State1xDN <= U1xDV;
      State2xDN <= U2xDV;
      State3xDN <= U3xDV;
      State4xDN <= U4xDV;
    end if;

    ---------------------------------------------------------------------------
    -- part of bus interface
    ---------------------------------------------------------------------------

    if DP_WriteNoncexS = '1' then
      if DATA_BUS_WIDTH = 32 then
        if AddressxDI(1 downto 0) = "00" then
          State4xDN(31 downto 0) <= DataWritexDI;
        elsif AddressxDI(1 downto 0) = "01" then
          State4xDN(63 downto 32) <= DataWritexDI;
        elsif AddressxDI(1 downto 0) = "10" then
          State3xDN(31 downto 0) <= DataWritexDI;
        else
          State3xDN(63 downto 32) <= DataWritexDI;
        end if;
      elsif DATA_BUS_WIDTH = 64 then
        if AddressxDI(0) = '0' then
          State3xDN <= DataWritexDI;
        else
          State4xDN <= DataWritexDI;
        end if;
      else
      -- TODO: implement for 16-bit, and 8-bit bus width
      end if;
    end if;

    if DP_WriteIODataxS = '1' then
      if DATA_BUS_WIDTH = 32 then
        if AddressxDI(0) = '0' then
          IODataxDN(31 downto 0) <= DataWritexDI;
        else
          IODataxDN(63 downto 32) <= DataWritexDI;
        end if;
      elsif DATA_BUS_WIDTH = 64 then
        IODataxDN <= DataWritexDI;
      else
      -- TODO: implement for 16-bit, and 8-bit bus width
      end if;
    end if;

  end process DatapathProc;
  

end architecture structural;
