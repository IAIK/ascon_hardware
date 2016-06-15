-------------------------------------------------------------------------------
-- Title      : A low-area implementation of Ascon, using shift registers
-- Project    :
-- Description: Implements Ascon Encryption and Decryption with < 4kGE area
--              and a power consumption of 93 uW @ 4 MHz (WC, 25°C) for the
--              90nm technology.
-------------------------------------------------------------------------------
-- File       : ascon_low_area.vhdl
-- Author     : Hannes Gross  <hannes.gross@iaik.tugraz.at>
-- Company    : 
-- Created    : 2014-05-20
-- Last update: 2014-05-26
-- Platform   : 
-- Standard   : VHDL'93/02
-------------------------------------------------------------------------------
-- Description:
-------------------------------------------------------------------------------
-- Copyright 2014 Graz University of Technology
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--     http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author           Description
-- 2014-05-20  1.0      Hannes Gross     Created
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.log2;
use ieee.math_real.ceil;

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

  constant CONTROL_STATE_SIZE       : integer := 4;
  constant STATE_WORD_SIZE          : integer := 64;

  constant CONST_KEY_SIZE : std_logic_vector(7 downto 0) := std_logic_vector(to_unsigned(KEY_SIZE, 8));
  constant CONST_ROUNDS_A : std_logic_vector(7 downto 0) := std_logic_vector(to_unsigned(ROUNDS_A, 8));
  constant CONST_ROUNDS_B : std_logic_vector(7 downto 0) := std_logic_vector(to_unsigned(ROUNDS_B, 8));

  -- Data path functions
  constant FUNCTION_PREPARE   : std_logic_vector(2 downto 0) := "000";
  constant FUNCTION_SBOX      : std_logic_vector(2 downto 0) := "001";
  constant FUNCTION_LINLAYER0 : std_logic_vector(2 downto 0) := "010";
  constant FUNCTION_LINLAYER1 : std_logic_vector(2 downto 0) := "011";
  constant FUNCTION_LINLAYER2 : std_logic_vector(2 downto 0) := "100";
  constant FUNCTION_LINLAYER3 : std_logic_vector(2 downto 0) := "101";
  constant FUNCTION_LINLAYER4 : std_logic_vector(2 downto 0) := "110";
  constant FUNCTION_POSTPROC  : std_logic_vector(2 downto 0) := "111";

  signal KeyxDP, KeyxDN                             : std_logic_vector(KEY_SIZE-1 downto 0);
  signal IODataxDP, IODataxDN                       : std_logic_vector(DATA_BLOCK_SIZE-1 downto 0);

  signal DP_WriteNoncexS  : std_logic;
  signal DP_WriteIODataxS : std_logic;

  signal CP_InitxSP, CP_InitxSN                               : std_logic;
  signal CP_AssociatexSP, CP_AssociatexSN                     : std_logic;
  signal CP_EncryptxSP, CP_EncryptxSN                         : std_logic;
  signal CP_DecryptxSP, CP_DecryptxSN                         : std_logic;
  signal CP_FinalAssociatexSP, CP_FinalAssociatexSN           : std_logic;
  signal CP_FinalEncryptxSP, CP_FinalEncryptxSN               : std_logic;
  signal CP_FinalDecryptxSP, CP_FinalDecryptxSN               : std_logic;
  signal CP_DonexSN, CP_DonexSP                               : std_logic;

  signal SboxINxS    :  std_logic_vector(4 downto 0);
  signal SboxOUTxS   :  std_logic_vector(4 downto 0);

  signal CountEnablexS         : std_logic;
  signal CounterRoundxD        : std_logic_vector(3 downto 0);
  signal CounterFunctSelxD     : std_logic_vector(2 downto 0);
  signal CounterSubIterationxD : std_logic_vector(5 downto 0);
  signal CounterResetxS        : std_logic;

  -- State and Temp registers
  signal X0INxD,  X1INxD,  X2INxD,  X3INxD,  X4INxD,  TempShiftRegINxD   : std_logic;
  signal X0OUTxD, X1OUTxD, X2OUTxD, X3OUTxD, X4OUTxD, TempShiftRegOUTxD  : std_logic_vector(STATE_WORD_SIZE-1 downto 0);
  signal ShiftEnablexS                                                   : std_logic_vector (5 downto 0);
  signal OverwriteENxS                                                   : std_logic;
  signal OverwriteDataxS                                                 : std_logic_vector(STATE_WORD_SIZE-1 downto 0);
  signal ResetX0xSB                                                      : std_logic;
  
  function ZEROS (
    constant WIDTH : natural)
    return std_logic_vector is
    variable x : std_logic_vector(WIDTH-1 downto 0);
  begin  -- ZEROS
    x := (others => '0');
    return x;
  end ZEROS;

  function ONES (
    constant WIDTH : natural)
    return std_logic_vector is
    variable x : std_logic_vector(WIDTH-1 downto 0);
  begin  -- ONES
    x := (others => '1');
    return x;
  end ONES;
  
begin  -- architecture structural

  ---- Instances
  -- State registers
  x4: entity work.ascon_shift_register
    generic map (
      DATA_WIDTH  => STATE_WORD_SIZE)
    port map (
      ClkxCI         => ClkxCI,
      RstxRBI        => RstxRBI,
      ShiftEnablexSI => ShiftEnablexS(4),
      ShiftRegINxDI  => X4INxD,
      ShiftRegOUTxDO => X4OUTxD);

    x3: entity work.ascon_shift_register
    generic map (
      DATA_WIDTH  => STATE_WORD_SIZE)
    port map (
      ClkxCI         => ClkxCI,
      RstxRBI        => RstxRBI,
      ShiftEnablexSI => ShiftEnablexS(3),
      ShiftRegINxDI  => X3INxD,
      ShiftRegOUTxDO => X3OUTxD);

    x2: entity work.ascon_shift_register
    generic map (
      DATA_WIDTH  => STATE_WORD_SIZE)
    port map (
      ClkxCI         => ClkxCI,
      RstxRBI        => RstxRBI,
      ShiftEnablexSI => ShiftEnablexS(2),
      ShiftRegINxDI  => X2INxD,
      ShiftRegOUTxDO => X2OUTxD);

    x1: entity work.ascon_shift_register
    generic map (
      DATA_WIDTH  => STATE_WORD_SIZE)
    port map (
      ClkxCI         => ClkxCI,
      RstxRBI        => RstxRBI,
      ShiftEnablexSI => ShiftEnablexS(1),
      ShiftRegINxDI  => X1INxD,
      ShiftRegOUTxDO => X1OUTxD);

    x0: entity work.ascon_shift_register
    generic map (
      RESET_VALUE => CONST_KEY_SIZE & CONST_ROUNDS_A & CONST_ROUNDS_B & ZEROS(64-3*8),
      DATA_WIDTH  => STATE_WORD_SIZE)
    port map (
      ClkxCI         => ClkxCI,
      RstxRBI        => ResetX0xSB,
      ShiftEnablexSI => ShiftEnablexS(0),
      ShiftRegINxDI  => X0INxD,
      ShiftRegOUTxDO => X0OUTxD);

  -- Temp shift register
    temp_shift_reg: entity work.ascon_shift_register_w_overwrite
    generic map (
      DATA_WIDTH  => STATE_WORD_SIZE)
    port map (
      ClkxCI           => ClkxCI,
      RstxRBI          => RstxRBI,
      OverwriteENxSI   => OverwriteENxS,
      OverwriteDataxSI => OverwriteDataxS,
      ShiftEnablexSI   => ShiftEnablexS(5),
      ShiftRegINxDI    => TempShiftRegINxD,
      ShiftRegOUTxDO   => TempShiftRegOUTxD);
  
  -- Sbox
  SboxINxS <= X4OUTxD(STATE_WORD_SIZE-1) & X3OUTxD(STATE_WORD_SIZE-1) & X2OUTxD(STATE_WORD_SIZE-1) & X1OUTxD(STATE_WORD_SIZE-1) & X0OUTxD(STATE_WORD_SIZE-1);
  ascon_sbox5_1: entity work.ascon_sbox5
    port map (
      SboxINxDI  => SboxINxS,
      SboxOUTxDO => SboxOUTxS);

  -- Counter
  ascon_counter_1: entity work.ascon_counter
    port map (
      ClkxCI                 => ClkxCI,
      RstxRBI                => CounterResetxS,
      CountEnablexSI         => CountEnablexS,
      CounterRoundxDO        => CounterRoundxD,
      CounterFunctSelxDO     => CounterFunctSelxD,
      CounterSubIterationxDO => CounterSubIterationxD);
  

  -- purpose: Defines all registers
  -- type   : sequential
  -- inputs : ClkxCI, RstxRBI, *xDN signals
  -- outputs: *xDP signals
  RegisterProc : process (ClkxCI, RstxRBI) is
  begin  -- process RegisterProc
    if RstxRBI = '0' then               -- asynchronous reset (active low)
      KeyxDP                    <= (others => '0');
      IODataxDP                 <= (others => '0');
      CP_InitxSP                <= '0';
      CP_AssociatexSP           <= '0';
      CP_EncryptxSP             <= '0';
      CP_DecryptxSP             <= '0';
      CP_FinalAssociatexSP      <= '0';
      CP_FinalEncryptxSP        <= '0';
      CP_FinalDecryptxSP        <= '0';
      CP_DonexSP                <= '1';
    elsif ClkxCI'event and ClkxCI = '1' then  -- rising clock edge
      KeyxDP                    <= KeyxDN;
      IODataxDP                 <= IODataxDN;
      CP_InitxSP                <= CP_InitxSN;
      CP_AssociatexSP           <= CP_AssociatexSN;
      CP_EncryptxSP             <= CP_EncryptxSN;
      CP_DecryptxSP             <= CP_DecryptxSN;
      CP_FinalAssociatexSP      <= CP_FinalAssociatexSN;
      CP_FinalEncryptxSP        <= CP_FinalEncryptxSN;
      CP_FinalDecryptxSP        <= CP_FinalDecryptxSN;
      CP_DonexSP                <= CP_DonexSN;
    end if;
  end process RegisterProc;


  -- purpose: Glue the internal registers with the bus
  -- type   : combinational
  DataBusLogicProc : process (AddressxDI, CP_AssociatexSP,
                              CP_DecryptxSP, CP_DonexSP, CP_EncryptxSP,
                              CP_FinalAssociatexSP, CP_FinalDecryptxSP,
                              CP_FinalEncryptxSP, CP_InitxSP, CSxSI,
                              DataWritexDI, IODataxDP, KeyxDP,
                              WExSI,
                              X3OUTxD,
                              X4OUTxD) is
    variable AddressxDV : integer;
    variable index      : integer;
  begin  -- process DataBusLogicProc
    KeyxDN <= KeyxDP;
    AddressxDV  := to_integer(unsigned(AddressxDI));
    index       := 0;
    DataReadxDO <= (others => '0');
    
    DP_WriteNoncexS  <= '0';
    DP_WriteIODataxS <= '0';

    CP_InitxSN           <= CP_InitxSP;
    CP_AssociatexSN      <= CP_AssociatexSP;
    CP_EncryptxSN        <= CP_EncryptxSP;
    CP_DecryptxSN        <= CP_DecryptxSP;
    CP_FinalEncryptxSN   <= CP_FinalEncryptxSP;
    CP_FinalDecryptxSN   <= CP_FinalDecryptxSP;
    CP_FinalAssociatexSN <= CP_FinalAssociatexSP;

    -- Reset signals when done with calculation
    if CP_DonexSP = '1' then
      CP_InitxSN           <= '0';
      CP_AssociatexSN      <= '0';
      CP_EncryptxSN        <= '0';
      CP_DecryptxSN        <= '0';
      CP_FinalEncryptxSN   <= '0';
      CP_FinalDecryptxSN   <= '0';
      CP_FinalAssociatexSN <= '0';
    end if;

    -- TODO: only designed for DATA_BUS_WIDTH=32
    if CSxSI = '1' then
      if WExSI = '1' then
        -- synchronous write
        if AddressxDV = 2 then
          -- command register
          CP_InitxSN           <= DataWritexDI(0);
          CP_AssociatexSN      <= DataWritexDI(1);
          CP_EncryptxSN        <= DataWritexDI(2);
          CP_DecryptxSN        <= DataWritexDI(3);
          CP_FinalEncryptxSN   <= DataWritexDI(4);
          CP_FinalDecryptxSN   <= DataWritexDI(5);
          CP_FinalAssociatexSN <= DataWritexDI(6);
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
          DataReadxDO(0) <= CP_InitxSP or CP_AssociatexSP or CP_FinalAssociatexSP or CP_EncryptxSP or CP_DecryptxSP or CP_FinalEncryptxSP or CP_FinalDecryptxSP;
        elsif (AddressxDV >= 12) and (AddressxDV < 14) then
          -- read the de/encrypted data and associated data
          index       := to_integer(unsigned(AddressxDI(0 downto 0)));
          DataReadxDO <= IODataxDP((index+1)*DATA_BUS_WIDTH-1 downto index*DATA_BUS_WIDTH);
        elsif (AddressxDV >= 16) and (AddressxDV < 20) then
          -- read the tag
          if AddressxDI(1 downto 0) = "00" then
            DataReadxDO <= X4OUTxD(31 downto 0);
          elsif AddressxDI(1 downto 0) = "01" then
            DataReadxDO <= X4OUTxD(63 downto 32);
          elsif AddressxDI(1 downto 0) = "10" then
            DataReadxDO <= X3OUTxD(31 downto 0);
          else
            DataReadxDO <= X3OUTxD(63 downto 32);
          end if;
        end if;
      end if;
    end if;
  end process DataBusLogicProc;

  -- purpose: Controlpath of Ascon
  -- type   : combinational
  ControlProc : process (CP_AssociatexSP, CP_DecryptxSP, CP_DonexSP,
                         CP_EncryptxSP, CP_FinalAssociatexSP,
                         CP_FinalDecryptxSP, CP_FinalEncryptxSP, CP_InitxSP,
                         CounterFunctSelxD, CounterRoundxD,
                         CounterRoundxD(3 downto 0), CounterSubIterationxD,
                         IODataxDP,
                         KeyxDP,
                         RstxRBI, SboxOUTxS,
                         TempShiftRegOUTxD, X0OUTxD,
                         X1OUTxD,
                         X2OUTxD,
                         X3OUTxD,
                         X4OUTxD) is
    variable RoundCounterxDV : integer;
    variable RoundConstxDV   : std_logic_vector(63 downto 0);
    variable ZerosOnexDV     : std_logic_vector(63 downto 0);
  begin  -- process ControlProc

    RoundConstxDV      :=  ZEROS(64-8) & not CounterRoundxD(3 downto 0) & CounterRoundxD(3 downto 0);
    ZerosOnexDV        :=  ZEROS(64-1) & '1';
    RoundCounterxDV    := to_integer(unsigned(CounterRoundxD));
    CountEnablexS      <= '0';
    CounterResetxS     <= '1';
    ShiftEnablexS      <= "000000";
    ResetX0xSB         <= RstxRBI;

    ----  State shift register input
    X0INxD <= X0OUTxD(STATE_WORD_SIZE-1); -- Per default, shift MSB in
    X1INxD <= X1OUTxD(STATE_WORD_SIZE-1); 
    X2INxD <= X2OUTxD(STATE_WORD_SIZE-1); 
    X3INxD <= X3OUTxD(STATE_WORD_SIZE-1);
    X4INxD <= X4OUTxD(STATE_WORD_SIZE-1);
    TempShiftRegINxD <= TempShiftRegOUTxD(STATE_WORD_SIZE-1);

    -- Enable counter when there is work to do
    if (CP_DonexSP = '0') and ((CP_InitxSP = '1') or (CP_AssociatexSP = '1') or (CP_FinalAssociatexSP = '1') or
       (CP_EncryptxSP = '1') or (CP_DecryptxSP = '1') or (CP_FinalEncryptxSP = '1') or (CP_FinalDecryptxSP = '1')) then
      CountEnablexS <= '1';
    end if;
    
    -- Reset Counter when finished with work cycle
    if (CP_DonexSP = '1') or (RstxRBI = '0') then
      CounterResetxS <= '0'; -- active low
      ShiftEnablexS  <= "000000";
    else
    -- Function Select Multiplexers
    if CounterFunctSelxD = FUNCTION_PREPARE then -- Preprocessing
      if(RoundCounterxDV = 0) then -- Initialization
        if (CP_InitxSP = '1') then
          ResetX0xSB <= '0';
          -- Prepare state k||a||b||0* (already done with reset) || K || N
          ShiftEnablexS        <= "111111";
          X1INxD <= KeyxDP(to_integer(127 - unsigned('0' & CounterSubIterationxD)));
          X2INxD <= KeyxDP(to_integer(63 - unsigned('0' & CounterSubIterationxD))) xor RoundConstxDV(to_integer(63 - unsigned('0' & CounterSubIterationxD)));
          X3INxD <= IODataxDP(to_integer(63 - unsigned('0' & CounterSubIterationxD)));
          X4INxD <= TempShiftRegOUTxD(STATE_WORD_SIZE-1); --to_integer(63 - unsigned('0' & CounterSubIterationxD)));
        elsif (CP_AssociatexSP = '1') or (CP_FinalAssociatexSP = '1') then
          -- XOR Associate Data with X0
          ShiftEnablexS(0) <= '1';
          X0INxD <= X0OUTxD(STATE_WORD_SIZE-1) xor IODataxDP(to_integer(63 - unsigned('0' & CounterSubIterationxD)));
          -- Add Round Constant
          ShiftEnablexS(2) <= '1';
          X2INxD <= X2OUTxD(STATE_WORD_SIZE-1) xor RoundConstxDV(to_integer(63 - unsigned('0' & CounterSubIterationxD)));
        elsif (CP_EncryptxSP = '1')  then
          -- Shift in Plaintext => x0
          ShiftEnablexS(0) <= '1';
          X0INxD <= X0OUTxD(STATE_WORD_SIZE-1) xor IODataxDP(to_integer(63 - unsigned('0' & CounterSubIterationxD)));
          -- Shift Encryption  result into Temp Register
          ShiftEnablexS(5) <= '1';
          TempShiftRegINxD <= X0OUTxD(STATE_WORD_SIZE-1) xor IODataxDP(to_integer(63 - unsigned('0' & CounterSubIterationxD)));
          -- Add Round Constant
          ShiftEnablexS(2) <= '1';
          X2INxD <= X2OUTxD(STATE_WORD_SIZE-1) xor RoundConstxDV(to_integer(63 - unsigned('0' & CounterSubIterationxD)));
        elsif (CP_DecryptxSP = '1')  then
          -- Shift in Cyphertext => x0
          ShiftEnablexS(0) <= '1';
          X0INxD <= IODataxDP(to_integer(63 - unsigned('0' & CounterSubIterationxD)));
          -- Shift Decryption result into Temp Register
          ShiftEnablexS(5) <= '1';
          TempShiftRegINxD <= X0OUTxD(STATE_WORD_SIZE-1) xor IODataxDP(to_integer(63 - unsigned('0' & CounterSubIterationxD)));
          -- Add Round Constant
          ShiftEnablexS(2) <= '1';
          X2INxD <= X2OUTxD(STATE_WORD_SIZE-1) xor RoundConstxDV(to_integer(63 - unsigned('0' & CounterSubIterationxD)));
        elsif (CP_FinalEncryptxSP = '1') then
          -- Shift in Plaintext => x0
          ShiftEnablexS(0) <= '1';
          X0INxD <= X0OUTxD(STATE_WORD_SIZE-1) xor IODataxDP(to_integer(63 - unsigned('0' & CounterSubIterationxD)));
          -- Shift Encryption  result into Temp Register
          ShiftEnablexS(5) <= '1';
          TempShiftRegINxD <= X0OUTxD(STATE_WORD_SIZE-1) xor IODataxDP(to_integer(63 - unsigned('0' & CounterSubIterationxD)));
          -- Add Round Constant and Key
          ShiftEnablexS(1) <= '1';
          ShiftEnablexS(2) <= '1';
          X1INxD <= X1OUTxD(STATE_WORD_SIZE-1) xor KeyxDP(to_integer(127 - unsigned('0' & CounterSubIterationxD)));
          X2INxD <= X2OUTxD(STATE_WORD_SIZE-1) xor KeyxDP(to_integer(63 - unsigned('0' & CounterSubIterationxD))) xor RoundConstxDV(to_integer(63 - unsigned('0' & CounterSubIterationxD)));
        elsif (CP_FinalDecryptxSP = '1') then
          -- Shift in Cyphertext => x0
          ShiftEnablexS(0) <= '1';
          X0INxD <= IODataxDP(to_integer(63 - unsigned('0' & CounterSubIterationxD)));
          -- Shift Decryption result into Temp Register
          ShiftEnablexS(5) <= '1';
          TempShiftRegINxD <= X0OUTxD(STATE_WORD_SIZE-1) xor IODataxDP(to_integer(63 - unsigned('0' & CounterSubIterationxD)));
          -- Add Round Constant and Key
          ShiftEnablexS(1) <= '1';
          ShiftEnablexS(2) <= '1';
          X1INxD <= X1OUTxD(STATE_WORD_SIZE-1) xor KeyxDP(to_integer(127 - unsigned('0' & CounterSubIterationxD)));
          X2INxD <= X2OUTxD(STATE_WORD_SIZE-1) xor KeyxDP(to_integer(63 - unsigned('0' & CounterSubIterationxD))) xor RoundConstxDV(to_integer(63 - unsigned('0' & CounterSubIterationxD)));
        end if;
      else
        ShiftEnablexS(2) <= '1';
        X2INxD <= X2OUTxD(STATE_WORD_SIZE-1) xor RoundConstxDV(to_integer(63 - unsigned('0' & CounterSubIterationxD)));
      end if;
    elsif CounterFunctSelxD = FUNCTION_SBOX then -- SBOX
      ShiftEnablexS <= "011111"; -- Shift all registers except Temp
      X0INxD <= SboxOUTxS(0);
      X1INxD <= SboxOUTxS(1);
      X2INxD <= SboxOUTxS(2);
      X3INxD <= SboxOUTxS(3);
      X4INxD <= SboxOUTxS(4);
    elsif CounterFunctSelxD = FUNCTION_LINLAYER0 then -- Linear Layer X0
      ShiftEnablexS(0) <= '1'; -- x0
      ShiftEnablexS(5) <= '1'; -- temp <= x0 xor ...
      TempShiftRegINxD <= X0OUTxD(STATE_WORD_SIZE-1) xor X0OUTxD(19 -1) xor X0OUTxD(28 -1);   
    elsif CounterFunctSelxD = FUNCTION_LINLAYER1 then -- Linear Layer X1
      ShiftEnablexS(0) <= '1'; -- x0 <= temp
      ShiftEnablexS(1) <= '1'; -- x1
      ShiftEnablexS(5) <= '1'; -- temp <= x1 xor....
      TempShiftRegINxD <= X1OUTxD(STATE_WORD_SIZE-1) xor X1OUTxD(61 -1) xor X1OUTxD(39 -1);
      X0INxD           <= TempShiftRegOUTxD(STATE_WORD_SIZE-1);
    elsif CounterFunctSelxD = FUNCTION_LINLAYER2 then -- Linear Layer X2
      ShiftEnablexS(1) <= '1'; -- x1 <= temp
      ShiftEnablexS(2) <= '1'; -- x2 
      ShiftEnablexS(5) <= '1'; -- temp <= x2 xor....
      TempShiftRegINxD <= X2OUTxD(STATE_WORD_SIZE-1) xor X2OUTxD(1 -1) xor X2OUTxD(6 -1);
      X1INxD           <= TempShiftRegOUTxD(STATE_WORD_SIZE-1);
    elsif CounterFunctSelxD = FUNCTION_LINLAYER3 then -- Linear Layer X3
      ShiftEnablexS(2) <= '1'; -- x2  <= temp
      ShiftEnablexS(3) <= '1'; -- x3
      ShiftEnablexS(5) <= '1'; -- temp <= x3 xor....
      TempShiftRegINxD <= X3OUTxD(STATE_WORD_SIZE-1) xor X3OUTxD(10 -1) xor X3OUTxD(17 -1);
      X2INxD           <= TempShiftRegOUTxD(STATE_WORD_SIZE-1);
    elsif CounterFunctSelxD = FUNCTION_LINLAYER4 then -- Linear Layer X4
      ShiftEnablexS(3) <= '1'; -- x3  <= temp
      ShiftEnablexS(4) <= '1'; -- x4
      ShiftEnablexS(5) <= '1'; -- temp <= x4 xor....
      TempShiftRegINxD <= X4OUTxD(STATE_WORD_SIZE-1) xor X4OUTxD(7 -1) xor X4OUTxD(41 -1);
      X3INxD           <= TempShiftRegOUTxD(STATE_WORD_SIZE-1);
    elsif CounterFunctSelxD = FUNCTION_POSTPROC then -- Post Processing
      ShiftEnablexS(4) <= '1'; -- x4  <= temp
      ShiftEnablexS(5) <= '1';
      X4INxD           <= TempShiftRegOUTxD(STATE_WORD_SIZE-1);
      -- Add Key or Constants 
      if (CP_InitxSP = '1') and (RoundCounterxDV = ROUNDS_A-1) then
        -- Add 0*||K
        ShiftEnablexS(3) <= '1';
        ShiftEnablexS(4) <= '1';
        X3INxD <= X3OUTxD(STATE_WORD_SIZE-1)           xor KeyxDP(to_integer(127 - unsigned('0' & CounterSubIterationxD)));
        X4INxD <= TempShiftRegOUTxD(STATE_WORD_SIZE-1) xor KeyxDP(to_integer( 63 - unsigned('0' & CounterSubIterationxD)));
      elsif (CP_FinalAssociatexSP = '1')  and (RoundCounterxDV = ROUNDS_B-1) then
        -- Add 0* || 1
        ShiftEnablexS(4) <= '1';
        X4INxD <= TempShiftRegOUTxD(STATE_WORD_SIZE-1) xor ZerosOnexDV(to_integer( 63 - unsigned('0' & CounterSubIterationxD)));
      elsif (CP_FinalEncryptxSP = '1' or CP_FinalDecryptxSP = '1') and (RoundCounterxDV = ROUNDS_A-1) then
        -- Add K => Tag
        ShiftEnablexS(3) <= '1';
        ShiftEnablexS(4) <= '1';
        X3INxD <= X3OUTxD(STATE_WORD_SIZE-1)           xor KeyxDP(to_integer(127 - unsigned('0' & CounterSubIterationxD)));
        X4INxD <= TempShiftRegOUTxD(STATE_WORD_SIZE-1) xor KeyxDP(to_integer( 63 - unsigned('0' & CounterSubIterationxD)));
      end if;  
    end if;
  end if;


    -- Done with calculation cycle
    if ((CP_InitxSP = '1') or (CP_FinalEncryptxSP = '1') or (CP_FinalDecryptxSP = '1')) and (RoundCounterxDV = ROUNDS_A-1)
      and CounterFunctSelxD = "111" and CounterSubIterationxD = "111111" then -- Finished
      CP_DonexSN            <= '1';
    elsif ((CP_AssociatexSP = '1') or (CP_FinalAssociatexSP = '1') or (CP_EncryptxSP = '1') or (CP_DecryptxSP = '1')) and (RoundCounterxDV = ROUNDS_B-1)
      and CounterFunctSelxD = "111"  and CounterSubIterationxD = "111111" then -- Finished
      CP_DonexSN            <= '1';
    else
      CP_DonexSN            <= '0';
    end if;
  end process ControlProc;

  --purpose: Datapath of Ascon
  --type   : combinational
  DatapathProc : process (AddressxDI(0), AddressxDI(1 downto 0), CP_DecryptxSP,
                          CP_EncryptxSP, CP_FinalDecryptxSP,
                          CP_FinalEncryptxSP, CounterFunctSelxD,
                          CounterRoundxD, CounterSubIterationxD,
                          DP_WriteIODataxS, DP_WriteNoncexS, DataWritexDI,
                          IODataxDP, TempShiftRegOUTxD,
                          TempShiftRegOUTxD(31 downto 0),
                          TempShiftRegOUTxD(63 downto 32)) is
    -- Variables
    begin  -- process DatapathProc

    ---------------------------------------------------------------------------
    -- part of bus interface
    ---------------------------------------------------------------------------
   IODataxDN <= IODataxDP; 
   OverwriteENxS      <= '0'; -- Controlls the temp registers overwrite function
   OverwriteDataxS    <= DataWritexDI & TempShiftRegOUTxD(31 downto 0);

   -- Sample data when Encryption is selected
   if((CP_EncryptxSP = '1') or (CP_FinalEncryptxSP = '1') or (CP_DecryptxSP = '1') or (CP_FinalDecryptxSP = '1')) then
     if (CounterRoundxD = ZEROS(4)) and (CounterFunctSelxD = FUNCTION_SBOX) and (CounterSubIterationxD = ZEROS(6)) then
       IODataxDN <= TempShiftRegOUTxD;
     end if;
   end if;
    -- Write Nonce
    if DP_WriteNoncexS = '1' then
      if DATA_BUS_WIDTH = 32 then
        if AddressxDI(1 downto 0) = "00" then
          OverwriteENxS   <= '1';
          OverwriteDataxS <=  TempShiftRegOUTxD(63 downto 32)  & DataWritexDI;
        elsif AddressxDI(1 downto 0) = "01" then
          OverwriteENxS   <= '1';
          OverwriteDataxS <=  DataWritexDI & TempShiftRegOUTxD(31 downto 0);
        elsif AddressxDI(1 downto 0) = "10" then
          IODataxDN(31 downto 0)   <= DataWritexDI;
        else
          IODataxDN(63 downto 32) <= DataWritexDI;
        end if;
      else
      -- TODO: implement for 16-bit, and 8-bit bus width
      end if;
    end if;
    -- Write IO Data = Associated Data or Plain/Cypher
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
