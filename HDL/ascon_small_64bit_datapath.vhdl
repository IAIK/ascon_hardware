-------------------------------------------------------------------------------
-- Title      : An area-optimized version of Ascon with a 64-bit datapath
-- Project    : Ascon
-------------------------------------------------------------------------------
-- File       : ascon_small_64bit_datapath.vhdl
-- Author     : Erich Wenger  <erich.wenger@iaik.tugraz.at>
-- Company    : Graz University of Technology
-- Created    : 2014-05-19
-- Last update: 2014-05-21
-- Platform   : ASIC design
-- Standard   : VHDL'93/02
-------------------------------------------------------------------------------
--   Copyright 2014 Graz University of Technology
--
--   Licensed under the Apache License, Version 2.0 (the "License");
--   you may not use this file except in compliance with the License.
--   You may obtain a copy of the License at
--
--       http://www.apache.org/licenses/LICENSE-2.0
--
--   Unless required by applicable law or agreed to in writing, software
--   distributed under the License is distributed on an "AS IS" BASIS,
--   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
--   See the License for the specific language governing permissions and
--   limitations under the License.
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author           Description
-- 2014-05-19  1.0      Erich Wenger     Created
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

  constant STATE_WORD_SIZE    : integer := 64;
  constant STATE_MACHINE_BITS : integer := 8;
  constant ROUND_COUNTER_BITS : integer := 4;

  constant CONST_KEY_SIZE : std_logic_vector(7 downto 0) := std_logic_vector(to_unsigned(KEY_SIZE, 8));
  constant CONST_ROUNDS_A : std_logic_vector(7 downto 0) := std_logic_vector(to_unsigned(ROUNDS_A, 8));
  constant CONST_ROUNDS_B : std_logic_vector(7 downto 0) := std_logic_vector(to_unsigned(ROUNDS_B, 8));

  signal KeyxDP, KeyxDN       : std_logic_vector(KEY_SIZE-1 downto 0);
  signal IODataxDP, IODataxDN : std_logic_vector(DATA_BLOCK_SIZE-1 downto 0);
  signal State0xDP, State0xDN : std_logic_vector(STATE_WORD_SIZE-1 downto 0);
  signal State1xDP, State1xDN : std_logic_vector(STATE_WORD_SIZE-1 downto 0);
  signal State2xDP, State2xDN : std_logic_vector(STATE_WORD_SIZE-1 downto 0);
  signal State3xDP, State3xDN : std_logic_vector(STATE_WORD_SIZE-1 downto 0);
  signal State4xDP, State4xDN : std_logic_vector(STATE_WORD_SIZE-1 downto 0);
  signal Temp0xDP, Temp0xDN   : std_logic_vector(STATE_WORD_SIZE-1 downto 0);
  signal Temp1xDP, Temp1xDN   : std_logic_vector(STATE_WORD_SIZE-1 downto 0);

  signal StatexDP : std_logic_vector(5*STATE_WORD_SIZE-1 downto 0);

  signal StateMachinexDP, StateMachinexDN : std_logic_vector(STATE_MACHINE_BITS-1 downto 0);
  signal RoundCounterxDP, RoundCounterxDN : std_logic_vector(ROUND_COUNTER_BITS-1 downto 0);

  constant STATE_ROUND_OP       : integer := 4;
  constant STATE_AFTER_ROUND_OP : integer := STATE_ROUND_OP + 59;

  signal DP_OpASelxS      : std_logic_vector(3 downto 0);
  signal DP_OpBSelxS      : std_logic_vector(3 downto 0);
  signal DP_OperationxS   : std_logic_vector(3 downto 0);
  signal DP_DestinationxS : std_logic_vector(3 downto 0);

  signal DP_ALU_ResultxD : std_logic_vector(STATE_WORD_SIZE-1 downto 0);

  constant DP_OPERAND_SEL_ZERO        : std_logic_vector(3 downto 0) := "0000";
  constant DP_OPERAND_SEL_STATE0      : std_logic_vector(3 downto 0) := "1000";
  constant DP_OPERAND_SEL_STATE1      : std_logic_vector(3 downto 0) := "1001";
  constant DP_OPERAND_SEL_STATE2      : std_logic_vector(3 downto 0) := "1010";
  constant DP_OPERAND_SEL_STATE3      : std_logic_vector(3 downto 0) := "1011";
  constant DP_OPERAND_SEL_STATE4      : std_logic_vector(3 downto 0) := "1100";
  constant DP_OPERAND_SEL_KEY0        : std_logic_vector(3 downto 0) := "1101";
  constant DP_OPERAND_SEL_KEY1        : std_logic_vector(3 downto 0) := "1110";
  constant DP_OPERAND_SEL_CONST_ONE   : std_logic_vector(3 downto 0) := "0001";
  constant DP_OPERAND_SEL_CONST_INIT  : std_logic_vector(3 downto 0) := "0010";
  constant DP_OPERAND_SEL_CONST_ROUND : std_logic_vector(3 downto 0) := "0011";
  constant DP_OPERAND_SEL_IODATA      : std_logic_vector(3 downto 0) := "0100";
  constant DP_OPERAND_SEL_TEMP0       : std_logic_vector(3 downto 0) := "0110";
  constant DP_OPERAND_SEL_TEMP1       : std_logic_vector(3 downto 0) := "0111";

  constant DP_OPERATION_XOR      : std_logic_vector(3 downto 0) := "0000";
  constant DP_OPERATION_NOT_AND  : std_logic_vector(3 downto 0) := "0001";
  constant DP_OPERATION_NOT      : std_logic_vector(3 downto 0) := "0010";
  constant DP_OPERATION_BUS_LOW  : std_logic_vector(3 downto 0) := "0100";
  constant DP_OPERATION_BUS_HIGH : std_logic_vector(3 downto 0) := "0101";
  constant DP_OPERATION_ROT1     : std_logic_vector(3 downto 0) := "1001";
  constant DP_OPERATION_ROT2     : std_logic_vector(3 downto 0) := "1010";
  constant DP_OPERATION_ROT4     : std_logic_vector(3 downto 0) := "1011";
  constant DP_OPERATION_ROT8     : std_logic_vector(3 downto 0) := "1100";
  constant DP_OPERATION_ROT16    : std_logic_vector(3 downto 0) := "1101";
  constant DP_OPERATION_ROT32    : std_logic_vector(3 downto 0) := "1110";

  constant DP_DESTINATION_NONE   : std_logic_vector(3 downto 0) := "0000";
  constant DP_DESTINATION_STATE0 : std_logic_vector(3 downto 0) := "1000";
  constant DP_DESTINATION_STATE1 : std_logic_vector(3 downto 0) := "1001";
  constant DP_DESTINATION_STATE2 : std_logic_vector(3 downto 0) := "1010";
  constant DP_DESTINATION_STATE3 : std_logic_vector(3 downto 0) := "1011";
  constant DP_DESTINATION_STATE4 : std_logic_vector(3 downto 0) := "1100";
  constant DP_DESTINATION_IODATA : std_logic_vector(3 downto 0) := "0100";
  constant DP_DESTINATION_TEMP0  : std_logic_vector(3 downto 0) := "0110";
  constant DP_DESTINATION_TEMP1  : std_logic_vector(3 downto 0) := "0111";

  signal CP_FinishedxS                : std_logic;
  signal CP_IdlexS                    : std_logic;
  signal CP_CommandDirectxS           : std_logic_vector(2 downto 0);
  signal CP_CommandxSN, CP_CommandxSP : std_logic_vector(4 downto 0);

  constant CP_DIRECT_NONE       : std_logic_vector(2 downto 0) := "000";
  constant CP_DIRECT_WR_IODATA0 : std_logic_vector(2 downto 0) := "010";
  constant CP_DIRECT_WR_IODATA1 : std_logic_vector(2 downto 0) := "011";
  constant CP_DIRECT_WR_NONCE0  : std_logic_vector(2 downto 0) := "100";
  constant CP_DIRECT_WR_NONCE1  : std_logic_vector(2 downto 0) := "101";
  constant CP_DIRECT_WR_NONCE2  : std_logic_vector(2 downto 0) := "110";
  constant CP_DIRECT_WR_NONCE3  : std_logic_vector(2 downto 0) := "111";

  constant CP_CMD_NONE               : std_logic_vector(4 downto 0) := "00000";
  constant CP_CMD_INIT               : std_logic_vector(4 downto 0) := "00001";
  constant CP_CMD_ASSOCIATE          : std_logic_vector(4 downto 0) := "01000";
  constant CP_CMD_ENCRYPT            : std_logic_vector(4 downto 0) := "01001";
  constant CP_CMD_DECRYPT            : std_logic_vector(4 downto 0) := "01010";
  constant CP_CMD_FINALIZE_ASSOCIATE : std_logic_vector(4 downto 0) := "01100";
  constant CP_CMD_FINAL_ENCRYPT      : std_logic_vector(4 downto 0) := "01101";
  constant CP_CMD_FINAL_DECRYPT      : std_logic_vector(4 downto 0) := "01110";
  constant CP_CMD_RD_IODATA0         : std_logic_vector(4 downto 0) := "10110";
  constant CP_CMD_RD_IODATA1         : std_logic_vector(4 downto 0) := "10111";
  constant CP_CMD_RD_TAG0            : std_logic_vector(4 downto 0) := "11000";
  constant CP_CMD_RD_TAG1            : std_logic_vector(4 downto 0) := "11001";
  constant CP_CMD_RD_TAG2            : std_logic_vector(4 downto 0) := "11010";
  constant CP_CMD_RD_TAG3            : std_logic_vector(4 downto 0) := "11011";

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
      KeyxDP          <= (others => '0');
      IODataxDP       <= (others => '0');
      State0xDP       <= (others => '0');
      State1xDP       <= (others => '0');
      State2xDP       <= (others => '0');
      State3xDP       <= (others => '0');
      State4xDP       <= (others => '0');
      StateMachinexDP <= (others => '0');
      RoundCounterxDP <= (others => '0');
      CP_CommandxSP   <= (others => '0');
      Temp0xDP        <= (others => '0');
      Temp1xDP        <= (others => '0');
    elsif ClkxCI'event and ClkxCI = '1' then  -- rising clock edge
      KeyxDP          <= KeyxDN;
      IODataxDP       <= IODataxDN;
      State0xDP       <= State0xDN;
      State1xDP       <= State1xDN;
      State2xDP       <= State2xDN;
      State3xDP       <= State3xDN;
      State4xDP       <= State4xDN;
      StateMachinexDP <= StateMachinexDN;
      RoundCounterxDP <= RoundCounterxDN;
      CP_CommandxSP   <= CP_CommandxSN;
      Temp0xDP        <= Temp0xDN;
      Temp1xDP        <= Temp1xDN;
    end if;
  end process RegisterProc;


  -- purpose: Glue the internal registers with the bus
  -- type   : combinational
  DataBusLogicProc : process (AddressxDI, CP_CommandxSP, CP_FinishedxS,
                              CP_IdlexS, CSxSI, DP_ALU_ResultxD, DataWritexDI,
                              KeyxDP, WExSI) is
    variable AddressxDV : integer;
    variable index      : integer;
  begin  -- process DataBusLogicProc
    KeyxDN        <= KeyxDP;
    CP_CommandxSN <= CP_CommandxSP;

    AddressxDV  := to_integer(unsigned(AddressxDI));
    index       := 0;
    DataReadxDO <= (others => '0');

    if CP_FinishedxS = '1' then
      CP_CommandxSN <= CP_CMD_NONE;
    end if;

    CP_CommandDirectxS <= CP_DIRECT_NONE;

    -- TODO: only designed for DATA_BUS_WIDTH=32
    if CSxSI = '1' then
      if WExSI = '1' then
        -- synchronous write
        if AddressxDV = 2 then
          -- command register
          if DataWritexDI(0) = '1' then
            CP_CommandxSN <= CP_CMD_INIT;
          end if;
          if DataWritexDI(1) = '1' then
            CP_CommandxSN <= CP_CMD_ASSOCIATE;
          end if;
          if DataWritexDI(2) = '1' then
            CP_CommandxSN <= CP_CMD_ENCRYPT;
          end if;
          if DataWritexDI(3) = '1' then
            CP_CommandxSN <= CP_CMD_DECRYPT;
          end if;
          if DataWritexDI(4) = '1' then
            CP_CommandxSN <= CP_CMD_FINAL_ENCRYPT;
          end if;
          if DataWritexDI(5) = '1' then
            CP_CommandxSN <= CP_CMD_FINAL_DECRYPT;
          end if;
          if DataWritexDI(6) = '1' then
            CP_CommandxSN <= CP_CMD_FINALIZE_ASSOCIATE;
          end if;
        elsif (AddressxDV >= 4) and (AddressxDV < 8) then
          -- write the key
          index                                                          := to_integer(unsigned(AddressxDI(1 downto 0)));
          KeyxDN((index+1)*DATA_BUS_WIDTH-1 downto index*DATA_BUS_WIDTH) <= DataWritexDI;
        elsif (AddressxDV >= 8) and (AddressxDV < 12) then
          -- write the nonce
          if AddressxDV = 8 then
            CP_CommandDirectxS <= CP_DIRECT_WR_NONCE0;
          elsif AddressxDV = 9 then
            CP_CommandDirectxS <= CP_DIRECT_WR_NONCE1;
          elsif AddressxDV = 10 then
            CP_CommandDirectxS <= CP_DIRECT_WR_NONCE2;
          elsif AddressxDV = 11 then
            CP_CommandDirectxS <= CP_DIRECT_WR_NONCE3;
          end if;
        elsif (AddressxDV >= 12) and (AddressxDV < 14) then
          -- write the data to de/encrypt and associated data
          if AddressxDV = 12 then
            CP_CommandDirectxS <= CP_DIRECT_WR_IODATA0;
          else
            CP_CommandDirectxS <= CP_DIRECT_WR_IODATA1;
          end if;
        end if;
      else
        -- asynchronous read
        if AddressxDV = 0 then
          DataReadxDO <= x"deadbeef";
        elsif AddressxDV = 1 then
          -- status register
          -- returns 1 if busy
          DataReadxDO(0) <= not CP_IdlexS;
        elsif (AddressxDV >= 12) and (AddressxDV < 20) then
          if AddressxDV = 12 then
            -- read the de/encrypted data and associated data
            CP_CommandxSN <= CP_CMD_RD_IODATA0;
          elsif AddressxDV = 13 then
            CP_CommandxSN <= CP_CMD_RD_IODATA1;
          elsif AddressxDV = 16 then
            -- read the tag
            CP_CommandxSN <= CP_CMD_RD_TAG0;
          elsif AddressxDV = 17 then
            CP_CommandxSN <= CP_CMD_RD_TAG1;
          elsif AddressxDV = 18 then
            CP_CommandxSN <= CP_CMD_RD_TAG2;
          elsif AddressxDV = 19 then
            CP_CommandxSN <= CP_CMD_RD_TAG3;
          end if;
          DataReadxDO <= DP_ALU_ResultxD(DATA_BUS_WIDTH-1 downto 0);
        end if;
      end if;
    end if;
  end process DataBusLogicProc;

  -- purpose: Controlpath of Ascon
  -- type   : combinational
  ControlProc : process (CP_CommandDirectxS, CP_CommandxSP, RoundCounterxDP,
                         StateMachinexDP) is
    variable StateMachinexDV : integer;
    variable RoundCounterxDV : integer;
  begin  -- process ControlProc

    StateMachinexDN <= StateMachinexDP;
    RoundCounterxDN <= RoundCounterxDP;
    StateMachinexDV := to_integer(unsigned(StateMachinexDP));
    RoundCounterxDV := to_integer(unsigned(RoundCounterxDP));

    CP_IdlexS        <= '0';
    CP_FinishedxS    <= '0';
    DP_OpASelxS      <= DP_OPERAND_SEL_ZERO;
    DP_OpBSelxS      <= DP_OPERAND_SEL_ZERO;
    DP_OperationxS   <= DP_OPERATION_XOR;
    DP_DestinationxS <= DP_DESTINATION_NONE;

    if CP_CommandxSP = CP_CMD_NONE then
      StateMachinexDN <= (others => '0');
    else
      StateMachinexDN <= std_logic_vector(unsigned(StateMachinexDP) + 1);
    end if;

    if CP_CommandxSP = CP_CMD_NONE then
      CP_IdlexS <= '1';

      if CP_CommandDirectxS = CP_DIRECT_WR_NONCE0 then
        DP_OpASelxS      <= DP_OPERAND_SEL_STATE4;
        DP_OperationxS   <= DP_OPERATION_BUS_LOW;
        DP_DestinationxS <= DP_DESTINATION_STATE4;
      elsif CP_CommandDirectxS = CP_DIRECT_WR_NONCE1 then
        DP_OpASelxS      <= DP_OPERAND_SEL_STATE4;
        DP_OperationxS   <= DP_OPERATION_BUS_HIGH;
        DP_DestinationxS <= DP_DESTINATION_STATE4;
      elsif CP_CommandDirectxS = CP_DIRECT_WR_NONCE2 then
        DP_OpASelxS      <= DP_OPERAND_SEL_STATE3;
        DP_OperationxS   <= DP_OPERATION_BUS_LOW;
        DP_DestinationxS <= DP_DESTINATION_STATE3;
      elsif CP_CommandDirectxS = CP_DIRECT_WR_NONCE3 then
        DP_OpASelxS      <= DP_OPERAND_SEL_STATE3;
        DP_OperationxS   <= DP_OPERATION_BUS_HIGH;
        DP_DestinationxS <= DP_DESTINATION_STATE3;
      elsif CP_CommandDirectxS = CP_DIRECT_WR_IODATA0 then
        DP_OpASelxS      <= DP_OPERAND_SEL_IODATA;
        DP_OperationxS   <= DP_OPERATION_BUS_LOW;
        DP_DestinationxS <= DP_DESTINATION_IODATA;
      elsif CP_CommandDirectxS = CP_DIRECT_WR_IODATA1 then
        DP_OpASelxS      <= DP_OPERAND_SEL_IODATA;
        DP_OperationxS   <= DP_OPERATION_BUS_HIGH;
        DP_DestinationxS <= DP_DESTINATION_IODATA;
      end if;
      ---------------------------------------------------------------------------
    elsif CP_CommandxSP = CP_CMD_RD_IODATA0 then
      DP_OpASelxS    <= DP_OPERAND_SEL_IODATA;
      DP_OperationxS <= DP_OPERATION_XOR;
      StateMachinexDN <= (others => '0');
      CP_FinishedxS <= '1';
    elsif CP_CommandxSP = CP_CMD_RD_IODATA1 then
      DP_OpASelxS    <= DP_OPERAND_SEL_IODATA;
      DP_OperationxS <= DP_OPERATION_ROT32;
      StateMachinexDN <= (others => '0');
      CP_FinishedxS <= '1';
    elsif CP_CommandxSP = CP_CMD_RD_TAG0 then
      DP_OpASelxS    <= DP_OPERAND_SEL_STATE4;
      DP_OperationxS <= DP_OPERATION_XOR;
      StateMachinexDN <= (others => '0');
      CP_FinishedxS <= '1';
    elsif CP_CommandxSP = CP_CMD_RD_TAG1 then
      DP_OpASelxS    <= DP_OPERAND_SEL_STATE4;
      DP_OperationxS <= DP_OPERATION_ROT32;
      StateMachinexDN <= (others => '0');
      CP_FinishedxS <= '1';
    elsif CP_CommandxSP = CP_CMD_RD_TAG2 then
      DP_OpASelxS    <= DP_OPERAND_SEL_STATE3;
      DP_OperationxS <= DP_OPERATION_XOR;
      StateMachinexDN <= (others => '0');
      CP_FinishedxS <= '1';
    elsif CP_CommandxSP = CP_CMD_RD_TAG3 then
      DP_OpASelxS    <= DP_OPERAND_SEL_STATE3;
      DP_OperationxS <= DP_OPERATION_ROT32;
      StateMachinexDN <= (others => '0');
      CP_FinishedxS <= '1';
      ---------------------------------------------------------------------------
    elsif CP_CommandxSP = CP_CMD_INIT then
      if (StateMachinexDV = 0) then
        DP_OpBSelxS      <= DP_OPERAND_SEL_CONST_INIT;
        DP_OperationxS   <= DP_OPERATION_XOR;
        DP_DestinationxS <= DP_DESTINATION_STATE0;
      elsif (StateMachinexDV = 1) then
        DP_OpASelxS      <= DP_OPERAND_SEL_KEY1;
        DP_OperationxS   <= DP_OPERATION_XOR;
        DP_DestinationxS <= DP_DESTINATION_STATE1;
      elsif (StateMachinexDV = 2) then
        DP_OpASelxS      <= DP_OPERAND_SEL_KEY0;
        DP_OperationxS   <= DP_OPERATION_XOR;
        DP_DestinationxS <= DP_DESTINATION_STATE2;
        StateMachinexDN  <= std_logic_vector(to_unsigned(STATE_ROUND_OP, STATE_MACHINE_BITS));
      elsif (StateMachinexDV = STATE_AFTER_ROUND_OP + 0) then
        DP_OpASelxS      <= DP_OPERAND_SEL_STATE3;
        DP_OpBSelxS      <= DP_OPERAND_SEL_KEY1;
        DP_OperationxS   <= DP_OPERATION_XOR;
        DP_DestinationxS <= DP_DESTINATION_STATE3;
      elsif (StateMachinexDV = STATE_AFTER_ROUND_OP + 1) then
        DP_OpASelxS      <= DP_OPERAND_SEL_STATE4;
        DP_OpBSelxS      <= DP_OPERAND_SEL_KEY0;
        DP_OperationxS   <= DP_OPERATION_XOR;
        DP_DestinationxS <= DP_DESTINATION_STATE4;
        CP_FinishedxS       <= '1';
        StateMachinexDN  <= (others => '0');
      end if;
    elsif (CP_CommandxSP = CP_CMD_ASSOCIATE) then
      if (StateMachinexDV = 0) then
        DP_OpASelxS      <= DP_OPERAND_SEL_STATE0;
        DP_OpBSelxS      <= DP_OPERAND_SEL_IODATA;
        DP_OperationxS   <= DP_OPERATION_XOR;
        DP_DestinationxS <= DP_DESTINATION_STATE0;
        StateMachinexDN  <= std_logic_vector(to_unsigned(STATE_ROUND_OP, STATE_MACHINE_BITS));
      end if;
    elsif (CP_CommandxSP = CP_CMD_ENCRYPT) then
      if (StateMachinexDV = 0) then
        DP_OpASelxS      <= DP_OPERAND_SEL_STATE0;
        DP_OpBSelxS      <= DP_OPERAND_SEL_IODATA;
        DP_OperationxS   <= DP_OPERATION_XOR;
        DP_DestinationxS <= DP_DESTINATION_STATE0;
      elsif (StateMachinexDV = 1) then
        DP_OpASelxS      <= DP_OPERAND_SEL_STATE0;
        DP_OperationxS   <= DP_OPERATION_XOR;
        DP_DestinationxS <= DP_DESTINATION_IODATA;
        StateMachinexDN  <= std_logic_vector(to_unsigned(STATE_ROUND_OP, STATE_MACHINE_BITS));
      end if;
    elsif (CP_CommandxSP = CP_CMD_FINALIZE_ASSOCIATE) then
      if (StateMachinexDV = 0) then
        DP_OpASelxS      <= DP_OPERAND_SEL_STATE4;
        DP_OpBSelxS      <= DP_OPERAND_SEL_CONST_ONE;
        DP_OperationxS   <= DP_OPERATION_XOR;
        DP_DestinationxS <= DP_DESTINATION_STATE4;
        CP_FinishedxS       <= '1';
        StateMachinexDN  <= (others => '0');
      end if;
    elsif (CP_CommandxSP = CP_CMD_FINAL_ENCRYPT) then
      if (StateMachinexDV = 0) then
        DP_OpASelxS      <= DP_OPERAND_SEL_STATE0;
        DP_OpBSelxS      <= DP_OPERAND_SEL_IODATA;
        DP_OperationxS   <= DP_OPERATION_XOR;
        DP_DestinationxS <= DP_DESTINATION_STATE0;
      elsif (StateMachinexDV = 1) then
        DP_OpASelxS      <= DP_OPERAND_SEL_STATE0;
        DP_OperationxS   <= DP_OPERATION_XOR;
        DP_DestinationxS <= DP_DESTINATION_IODATA;
      elsif (StateMachinexDV = 2) then
        DP_OpASelxS      <= DP_OPERAND_SEL_STATE1;
        DP_OpBSelxS      <= DP_OPERAND_SEL_KEY1;
        DP_OperationxS   <= DP_OPERATION_XOR;
        DP_DestinationxS <= DP_DESTINATION_STATE1;
      elsif (StateMachinexDV = 3) then
        DP_OpASelxS      <= DP_OPERAND_SEL_STATE2;
        DP_OpBSelxS      <= DP_OPERAND_SEL_KEY0;
        DP_OperationxS   <= DP_OPERATION_XOR;
        DP_DestinationxS <= DP_DESTINATION_STATE2;
        StateMachinexDN  <= std_logic_vector(to_unsigned(STATE_ROUND_OP, STATE_MACHINE_BITS));
      elsif (StateMachinexDV = STATE_AFTER_ROUND_OP + 0) then
        DP_OpASelxS      <= DP_OPERAND_SEL_STATE3;
        DP_OpBSelxS      <= DP_OPERAND_SEL_KEY1;
        DP_OperationxS   <= DP_OPERATION_XOR;
        DP_DestinationxS <= DP_DESTINATION_STATE3;
      elsif (StateMachinexDV = STATE_AFTER_ROUND_OP + 1) then
        DP_OpASelxS      <= DP_OPERAND_SEL_STATE4;
        DP_OpBSelxS      <= DP_OPERAND_SEL_KEY0;
        DP_OperationxS   <= DP_OPERATION_XOR;
        DP_DestinationxS <= DP_DESTINATION_STATE4;
        CP_FinishedxS       <= '1';
        StateMachinexDN  <= (others => '0');
      end if;
    elsif (CP_CommandxSP = CP_CMD_DECRYPT) then
      if (StateMachinexDV = 0) then
        DP_OpASelxS      <= DP_OPERAND_SEL_STATE0;
        DP_OpBSelxS      <= DP_OPERAND_SEL_IODATA;
        DP_OperationxS   <= DP_OPERATION_XOR;
        DP_DestinationxS <= DP_DESTINATION_IODATA;
      elsif (StateMachinexDV = 1) then
        DP_OpASelxS      <= DP_OPERAND_SEL_STATE0;
        DP_OpBSelxS      <= DP_OPERAND_SEL_IODATA;
        DP_OperationxS   <= DP_OPERATION_XOR;
        DP_DestinationxS <= DP_DESTINATION_STATE0;
        StateMachinexDN  <= std_logic_vector(to_unsigned(STATE_ROUND_OP, STATE_MACHINE_BITS));
      end if;
    elsif (CP_CommandxSP = CP_CMD_FINAL_DECRYPT) then
      if (StateMachinexDV = 0) then
        DP_OpASelxS      <= DP_OPERAND_SEL_STATE0;
        DP_OpBSelxS      <= DP_OPERAND_SEL_IODATA;
        DP_OperationxS   <= DP_OPERATION_XOR;
        DP_DestinationxS <= DP_DESTINATION_IODATA;
      elsif (StateMachinexDV = 1) then
        DP_OpASelxS      <= DP_OPERAND_SEL_STATE0;
        DP_OpBSelxS      <= DP_OPERAND_SEL_IODATA;
        DP_OperationxS   <= DP_OPERATION_XOR;
        DP_DestinationxS <= DP_DESTINATION_STATE0;
      elsif (StateMachinexDV = 2) then
        DP_OpASelxS      <= DP_OPERAND_SEL_STATE1;
        DP_OpBSelxS      <= DP_OPERAND_SEL_KEY1;
        DP_OperationxS   <= DP_OPERATION_XOR;
        DP_DestinationxS <= DP_DESTINATION_STATE1;
      elsif (StateMachinexDV = 3) then
        DP_OpASelxS      <= DP_OPERAND_SEL_STATE2;
        DP_OpBSelxS      <= DP_OPERAND_SEL_KEY0;
        DP_OperationxS   <= DP_OPERATION_XOR;
        DP_DestinationxS <= DP_DESTINATION_STATE2;
        StateMachinexDN  <= std_logic_vector(to_unsigned(STATE_ROUND_OP, STATE_MACHINE_BITS));
      elsif (StateMachinexDV = STATE_AFTER_ROUND_OP + 0) then
        DP_OpASelxS      <= DP_OPERAND_SEL_STATE3;
        DP_OpBSelxS      <= DP_OPERAND_SEL_KEY1;
        DP_OperationxS   <= DP_OPERATION_XOR;
        DP_DestinationxS <= DP_DESTINATION_STATE3;
      elsif (StateMachinexDV = STATE_AFTER_ROUND_OP + 1) then
        DP_OpASelxS      <= DP_OPERAND_SEL_STATE4;
        DP_OpBSelxS      <= DP_OPERAND_SEL_KEY0;
        DP_OperationxS   <= DP_OPERATION_XOR;
        DP_DestinationxS <= DP_DESTINATION_STATE4;
        CP_FinishedxS       <= '1';
        StateMachinexDN  <= (others => '0');
      end if;
    end if;

    if (CP_CommandxSP = CP_CMD_INIT) or
      (CP_CommandxSP = CP_CMD_ASSOCIATE) or
      (CP_CommandxSP = CP_CMD_ENCRYPT) or
      (CP_CommandxSP = CP_CMD_DECRYPT) or
      (CP_CommandxSP = CP_CMD_FINAL_ENCRYPT) or
      (CP_CommandxSP = CP_CMD_FINAL_DECRYPT) then
      if (StateMachinexDV = STATE_ROUND_OP + 0) then
        -- add the round constant
        DP_OpASelxS      <= DP_OPERAND_SEL_STATE2;
        DP_OpBSelxS      <= DP_OPERAND_SEL_CONST_ROUND;
        DP_OperationxS   <= DP_OPERATION_XOR;
        DP_DestinationxS <= DP_DESTINATION_STATE2;
      elsif (StateMachinexDV = STATE_ROUND_OP + 1) then
        -- perform the S-Box layer
        DP_OpASelxS      <= DP_OPERAND_SEL_STATE0;
        DP_OpBSelxS      <= DP_OPERAND_SEL_STATE4;
        DP_OperationxS   <= DP_OPERATION_XOR;
        DP_DestinationxS <= DP_DESTINATION_TEMP1;
      elsif (StateMachinexDV = STATE_ROUND_OP + 2) then
        DP_OpASelxS      <= DP_OPERAND_SEL_STATE3;
        DP_OpBSelxS      <= DP_OPERAND_SEL_STATE4;
        DP_OperationxS   <= DP_OPERATION_XOR;
        DP_DestinationxS <= DP_DESTINATION_TEMP0;
      elsif (StateMachinexDV = STATE_ROUND_OP + 3) then
        DP_OpASelxS      <= DP_OPERAND_SEL_STATE1;
        DP_OpBSelxS      <= DP_OPERAND_SEL_STATE2;
        DP_OperationxS   <= DP_OPERATION_XOR;
        DP_DestinationxS <= DP_DESTINATION_STATE2;
      elsif (StateMachinexDV = STATE_ROUND_OP + 4) then
        DP_OpASelxS      <= DP_OPERAND_SEL_STATE1;
        DP_OpBSelxS      <= DP_OPERAND_SEL_STATE2;
        DP_OperationxS   <= DP_OPERATION_NOT_AND;
        DP_DestinationxS <= DP_DESTINATION_STATE0;
      elsif (StateMachinexDV = STATE_ROUND_OP + 5) then
        DP_OpASelxS      <= DP_OPERAND_SEL_STATE0;
        DP_OpBSelxS      <= DP_OPERAND_SEL_TEMP1;
        DP_OperationxS   <= DP_OPERATION_XOR;
        DP_DestinationxS <= DP_DESTINATION_STATE0;
      elsif (StateMachinexDV = STATE_ROUND_OP + 6) then
        DP_OpASelxS      <= DP_OPERAND_SEL_STATE1;
        DP_OpBSelxS      <= DP_OPERAND_SEL_STATE2;
        DP_OperationxS   <= DP_OPERATION_NOT_AND;
        DP_DestinationxS <= DP_DESTINATION_STATE4;
      elsif (StateMachinexDV = STATE_ROUND_OP + 7) then
        DP_OpASelxS      <= DP_OPERAND_SEL_STATE0;
        DP_OpBSelxS      <= DP_OPERAND_SEL_STATE4;
        DP_OperationxS   <= DP_OPERATION_XOR;
        DP_DestinationxS <= DP_DESTINATION_STATE4;
      elsif (StateMachinexDV = STATE_ROUND_OP + 8) then
        DP_OpASelxS      <= DP_OPERAND_SEL_STATE4;
        DP_OpBSelxS      <= DP_OPERAND_SEL_STATE1;
        DP_OperationxS   <= DP_OPERATION_NOT_AND;
        DP_DestinationxS <= DP_DESTINATION_STATE4;
      elsif (StateMachinexDV = STATE_ROUND_OP + 9) then
        DP_OpASelxS      <= DP_OPERAND_SEL_TEMP0;
        DP_OpBSelxS      <= DP_OPERAND_SEL_STATE4;
        DP_OperationxS   <= DP_OPERATION_XOR;
        DP_DestinationxS <= DP_DESTINATION_STATE4;
      elsif (StateMachinexDV = STATE_ROUND_OP + 10) then
        DP_OpASelxS      <= DP_OPERAND_SEL_STATE1;
        DP_OpBSelxS      <= DP_OPERAND_SEL_STATE2;
        DP_OperationxS   <= DP_OPERATION_NOT_AND;
        DP_DestinationxS <= DP_DESTINATION_TEMP1;
      elsif (StateMachinexDV = STATE_ROUND_OP + 11) then
        DP_OpASelxS      <= DP_OPERAND_SEL_STATE0;
        DP_OpBSelxS      <= DP_OPERAND_SEL_TEMP1;
        DP_OperationxS   <= DP_OPERATION_XOR;
        DP_DestinationxS <= DP_DESTINATION_TEMP1;
      elsif (StateMachinexDV = STATE_ROUND_OP + 12) then
        DP_OpASelxS      <= DP_OPERAND_SEL_TEMP0;
        DP_OpBSelxS      <= DP_OPERAND_SEL_TEMP1;
        DP_OperationxS   <= DP_OPERATION_NOT_AND;
        DP_DestinationxS <= DP_DESTINATION_TEMP1;
      elsif (StateMachinexDV = STATE_ROUND_OP + 13) then
        DP_OpASelxS      <= DP_OPERAND_SEL_STATE3;
        DP_OpBSelxS      <= DP_OPERAND_SEL_TEMP1;
        DP_OperationxS   <= DP_OPERATION_XOR;
        DP_DestinationxS <= DP_DESTINATION_TEMP1;
      elsif (StateMachinexDV = STATE_ROUND_OP + 14) then
        DP_OpASelxS      <= DP_OPERAND_SEL_STATE1;
        DP_OpBSelxS      <= DP_OPERAND_SEL_STATE2;
        DP_OperationxS   <= DP_OPERATION_NOT_AND;
        DP_DestinationxS <= DP_DESTINATION_TEMP0;
      elsif (StateMachinexDV = STATE_ROUND_OP + 15) then
        DP_OpASelxS      <= DP_OPERAND_SEL_STATE0;
        DP_OpBSelxS      <= DP_OPERAND_SEL_TEMP0;
        DP_OperationxS   <= DP_OPERATION_XOR;
        DP_DestinationxS <= DP_DESTINATION_TEMP0;
      elsif (StateMachinexDV = STATE_ROUND_OP + 16) then
        DP_OpASelxS      <= DP_OPERAND_SEL_TEMP0;
        DP_OpBSelxS      <= DP_OPERAND_SEL_STATE1;
        DP_OperationxS   <= DP_OPERATION_NOT_AND;
        DP_DestinationxS <= DP_DESTINATION_TEMP0;
      elsif (StateMachinexDV = STATE_ROUND_OP + 17) then
        DP_OpASelxS      <= DP_OPERAND_SEL_STATE4;
        DP_OpBSelxS      <= DP_OPERAND_SEL_TEMP0;
        DP_OperationxS   <= DP_OPERATION_XOR;
        DP_DestinationxS <= DP_DESTINATION_TEMP0;
      elsif (StateMachinexDV = STATE_ROUND_OP + 18) then
        DP_OpASelxS      <= DP_OPERAND_SEL_STATE3;
        DP_OpBSelxS      <= DP_OPERAND_SEL_TEMP0;
        DP_OperationxS   <= DP_OPERATION_NOT_AND;
        DP_DestinationxS <= DP_DESTINATION_TEMP0;
      elsif (StateMachinexDV = STATE_ROUND_OP + 19) then
        DP_OpASelxS      <= DP_OPERAND_SEL_STATE2;
        DP_OpBSelxS      <= DP_OPERAND_SEL_TEMP0;
        DP_OperationxS   <= DP_OPERATION_XOR;
        DP_DestinationxS <= DP_DESTINATION_TEMP0;
      elsif (StateMachinexDV = STATE_ROUND_OP + 20) then
        DP_OpASelxS      <= DP_OPERAND_SEL_STATE2;
        DP_OpBSelxS      <= DP_OPERAND_SEL_STATE3;
        DP_OperationxS   <= DP_OPERATION_NOT_AND;
        DP_DestinationxS <= DP_DESTINATION_STATE2;
      elsif (StateMachinexDV = STATE_ROUND_OP + 21) then
        DP_OpASelxS      <= DP_OPERAND_SEL_STATE1;
        DP_OpBSelxS      <= DP_OPERAND_SEL_STATE2;
        DP_OperationxS   <= DP_OPERATION_XOR;
        DP_DestinationxS <= DP_DESTINATION_STATE1;
      elsif (StateMachinexDV = STATE_ROUND_OP + 22) then
        DP_OpASelxS      <= DP_OPERAND_SEL_STATE0;
        DP_OpBSelxS      <= DP_OPERAND_SEL_STATE1;
        DP_OperationxS   <= DP_OPERATION_XOR;
        DP_DestinationxS <= DP_DESTINATION_STATE1;
      elsif (StateMachinexDV = STATE_ROUND_OP + 23) then
        DP_OpASelxS      <= DP_OPERAND_SEL_STATE0;
        DP_OpBSelxS      <= DP_OPERAND_SEL_STATE4;
        DP_OperationxS   <= DP_OPERATION_XOR;
        DP_DestinationxS <= DP_DESTINATION_STATE0;
      elsif (StateMachinexDV = STATE_ROUND_OP + 24) then
        DP_OpASelxS      <= DP_OPERAND_SEL_TEMP0;
        DP_OpBSelxS      <= DP_OPERAND_SEL_TEMP1;
        DP_OperationxS   <= DP_OPERATION_XOR;
        DP_DestinationxS <= DP_DESTINATION_STATE3;
      elsif (StateMachinexDV = STATE_ROUND_OP + 25) then
        DP_OpASelxS      <= DP_OPERAND_SEL_TEMP0;
        DP_OperationxS   <= DP_OPERATION_NOT;
        DP_DestinationxS <= DP_DESTINATION_STATE2;
      elsif (StateMachinexDV = STATE_ROUND_OP + 26) then
        -- linear layer (State 0)
        DP_OpASelxS      <= DP_OPERAND_SEL_STATE0;
        DP_OperationxS   <= DP_OPERATION_ROT16;
        DP_DestinationxS <= DP_DESTINATION_TEMP0;
      elsif (StateMachinexDV = STATE_ROUND_OP + 27) then
        DP_OpASelxS      <= DP_OPERAND_SEL_TEMP0;
        DP_OperationxS   <= DP_OPERATION_ROT8;
        DP_DestinationxS <= DP_DESTINATION_TEMP1;
      elsif (StateMachinexDV = STATE_ROUND_OP + 28) then
        DP_OpASelxS      <= DP_OPERAND_SEL_TEMP1;
        DP_OperationxS   <= DP_OPERATION_ROT4;
        DP_DestinationxS <= DP_DESTINATION_TEMP1;
      elsif (StateMachinexDV = STATE_ROUND_OP + 29) then
        DP_OpASelxS      <= DP_OPERAND_SEL_TEMP0;
        DP_OperationxS   <= DP_OPERATION_ROT2;
        DP_DestinationxS <= DP_DESTINATION_TEMP0;
      elsif (StateMachinexDV = STATE_ROUND_OP + 30) then
        DP_OpASelxS      <= DP_OPERAND_SEL_TEMP0;
        DP_OperationxS   <= DP_OPERATION_ROT1;
        DP_DestinationxS <= DP_DESTINATION_TEMP0;
      elsif (StateMachinexDV = STATE_ROUND_OP + 31) then
        DP_OpASelxS      <= DP_OPERAND_SEL_STATE0;
        DP_OpBSelxS      <= DP_OPERAND_SEL_TEMP0;
        DP_OperationxS   <= DP_OPERATION_XOR;
        DP_DestinationxS <= DP_DESTINATION_STATE0;
      elsif (StateMachinexDV = STATE_ROUND_OP + 32) then
        DP_OpASelxS      <= DP_OPERAND_SEL_STATE0;
        DP_OpBSelxS      <= DP_OPERAND_SEL_TEMP1;
        DP_OperationxS   <= DP_OPERATION_XOR;
        DP_DestinationxS <= DP_DESTINATION_STATE0;
      elsif (StateMachinexDV = STATE_ROUND_OP + 33) then
        -- linear layer (State 1)
        DP_OpASelxS      <= DP_OPERAND_SEL_STATE1;
        DP_OperationxS   <= DP_OPERATION_ROT32;
        DP_DestinationxS <= DP_DESTINATION_TEMP0;
      elsif (StateMachinexDV = STATE_ROUND_OP + 34) then
        DP_OpASelxS      <= DP_OPERAND_SEL_TEMP0;
        DP_OperationxS   <= DP_OPERATION_ROT1;
        DP_DestinationxS <= DP_DESTINATION_TEMP0;
      elsif (StateMachinexDV = STATE_ROUND_OP + 35) then
        DP_OpASelxS      <= DP_OPERAND_SEL_TEMP0;
        DP_OperationxS   <= DP_OPERATION_ROT4;
        DP_DestinationxS <= DP_DESTINATION_TEMP0;
      elsif (StateMachinexDV = STATE_ROUND_OP + 36) then
        DP_OpASelxS      <= DP_OPERAND_SEL_TEMP0;
        DP_OperationxS   <= DP_OPERATION_ROT16;
        DP_DestinationxS <= DP_DESTINATION_TEMP1;
      elsif (StateMachinexDV = STATE_ROUND_OP + 37) then
        DP_OpASelxS      <= DP_OPERAND_SEL_TEMP1;
        DP_OperationxS   <= DP_OPERATION_ROT8;
        DP_DestinationxS <= DP_DESTINATION_TEMP1;
      elsif (StateMachinexDV = STATE_ROUND_OP + 38) then
        DP_OpASelxS      <= DP_OPERAND_SEL_TEMP0;
        DP_OperationxS   <= DP_OPERATION_ROT2;
        DP_DestinationxS <= DP_DESTINATION_TEMP0;
      elsif (StateMachinexDV = STATE_ROUND_OP + 39) then
        DP_OpASelxS      <= DP_OPERAND_SEL_STATE1;
        DP_OpBSelxS      <= DP_OPERAND_SEL_TEMP0;
        DP_OperationxS   <= DP_OPERATION_XOR;
        DP_DestinationxS <= DP_DESTINATION_STATE1;
      elsif (StateMachinexDV = STATE_ROUND_OP + 40) then
        DP_OpASelxS      <= DP_OPERAND_SEL_STATE1;
        DP_OpBSelxS      <= DP_OPERAND_SEL_TEMP1;
        DP_OperationxS   <= DP_OPERATION_XOR;
        DP_DestinationxS <= DP_DESTINATION_STATE1;
      elsif (StateMachinexDV = STATE_ROUND_OP + 41) then
        -- linear layer (State 2)
        DP_OpASelxS      <= DP_OPERAND_SEL_STATE2;
        DP_OperationxS   <= DP_OPERATION_ROT1;
        DP_DestinationxS <= DP_DESTINATION_TEMP0;
      elsif (StateMachinexDV = STATE_ROUND_OP + 42) then
        DP_OpASelxS      <= DP_OPERAND_SEL_STATE2;
        DP_OperationxS   <= DP_OPERATION_ROT2;
        DP_DestinationxS <= DP_DESTINATION_TEMP1;
      elsif (StateMachinexDV = STATE_ROUND_OP + 43) then
        DP_OpASelxS      <= DP_OPERAND_SEL_TEMP1;
        DP_OperationxS   <= DP_OPERATION_ROT4;
        DP_DestinationxS <= DP_DESTINATION_TEMP1;
      elsif (StateMachinexDV = STATE_ROUND_OP + 44) then
        DP_OpASelxS      <= DP_OPERAND_SEL_STATE2;
        DP_OpBSelxS      <= DP_OPERAND_SEL_TEMP0;
        DP_OperationxS   <= DP_OPERATION_XOR;
        DP_DestinationxS <= DP_DESTINATION_STATE2;
      elsif (StateMachinexDV = STATE_ROUND_OP + 45) then
        DP_OpASelxS      <= DP_OPERAND_SEL_STATE2;
        DP_OpBSelxS      <= DP_OPERAND_SEL_TEMP1;
        DP_OperationxS   <= DP_OPERATION_XOR;
        DP_DestinationxS <= DP_DESTINATION_STATE2;
      elsif (StateMachinexDV = STATE_ROUND_OP + 46) then
        -- linear layer (State 3)
        DP_OpASelxS      <= DP_OPERAND_SEL_STATE3;
        DP_OperationxS   <= DP_OPERATION_ROT2;
        DP_DestinationxS <= DP_DESTINATION_TEMP0;
      elsif (StateMachinexDV = STATE_ROUND_OP + 47) then
        DP_OpASelxS      <= DP_OPERAND_SEL_TEMP0;
        DP_OperationxS   <= DP_OPERATION_ROT8;
        DP_DestinationxS <= DP_DESTINATION_TEMP0;
      elsif (StateMachinexDV = STATE_ROUND_OP + 48) then
        DP_OpASelxS      <= DP_OPERAND_SEL_STATE3;
        DP_OperationxS   <= DP_OPERATION_ROT1;
        DP_DestinationxS <= DP_DESTINATION_TEMP1;
      elsif (StateMachinexDV = STATE_ROUND_OP + 49) then
        DP_OpASelxS      <= DP_OPERAND_SEL_TEMP1;
        DP_OperationxS   <= DP_OPERATION_ROT16;
        DP_DestinationxS <= DP_DESTINATION_TEMP1;
      elsif (StateMachinexDV = STATE_ROUND_OP + 50) then
        DP_OpASelxS      <= DP_OPERAND_SEL_STATE3;
        DP_OpBSelxS      <= DP_OPERAND_SEL_TEMP0;
        DP_OperationxS   <= DP_OPERATION_XOR;
        DP_DestinationxS <= DP_DESTINATION_STATE3;
      elsif (StateMachinexDV = STATE_ROUND_OP + 51) then
        DP_OpASelxS      <= DP_OPERAND_SEL_STATE3;
        DP_OpBSelxS      <= DP_OPERAND_SEL_TEMP1;
        DP_OperationxS   <= DP_OPERATION_XOR;
        DP_DestinationxS <= DP_DESTINATION_STATE3;
      elsif (StateMachinexDV = STATE_ROUND_OP + 52) then
        -- linear layer (State 4)
        DP_OpASelxS      <= DP_OPERAND_SEL_STATE4;
        DP_OperationxS   <= DP_OPERATION_ROT1;
        DP_DestinationxS <= DP_DESTINATION_TEMP0;
      elsif (StateMachinexDV = STATE_ROUND_OP + 53) then
        DP_OpASelxS      <= DP_OPERAND_SEL_TEMP0;
        DP_OperationxS   <= DP_OPERATION_ROT8;
        DP_DestinationxS <= DP_DESTINATION_TEMP1;
      elsif (StateMachinexDV = STATE_ROUND_OP + 54) then
        DP_OpASelxS      <= DP_OPERAND_SEL_TEMP1;
        DP_OperationxS   <= DP_OPERATION_ROT32;
        DP_DestinationxS <= DP_DESTINATION_TEMP1;
      elsif (StateMachinexDV = STATE_ROUND_OP + 55) then
        DP_OpASelxS      <= DP_OPERAND_SEL_TEMP0;
        DP_OperationxS   <= DP_OPERATION_ROT2;
        DP_DestinationxS <= DP_DESTINATION_TEMP0;
      elsif (StateMachinexDV = STATE_ROUND_OP + 56) then
        DP_OpASelxS      <= DP_OPERAND_SEL_TEMP0;
        DP_OperationxS   <= DP_OPERATION_ROT4;
        DP_DestinationxS <= DP_DESTINATION_TEMP0;
      elsif (StateMachinexDV = STATE_ROUND_OP + 57) then
        DP_OpASelxS      <= DP_OPERAND_SEL_STATE4;
        DP_OpBSelxS      <= DP_OPERAND_SEL_TEMP0;
        DP_OperationxS   <= DP_OPERATION_XOR;
        DP_DestinationxS <= DP_DESTINATION_STATE4;
      elsif (StateMachinexDV = STATE_ROUND_OP + 58) then
        DP_OpASelxS      <= DP_OPERAND_SEL_STATE4;
        DP_OpBSelxS      <= DP_OPERAND_SEL_TEMP1;
        DP_OperationxS   <= DP_OPERATION_XOR;
        DP_DestinationxS <= DP_DESTINATION_STATE4;

        if (CP_CommandxSP = CP_CMD_ENCRYPT) or
          (CP_CommandxSP = CP_CMD_DECRYPT) or
          (CP_CommandxSP = CP_CMD_ASSOCIATE) then
          if RoundCounterxDV = ROUNDS_B-1 then
            CP_FinishedxS      <= '1';
            StateMachinexDN <= (others => '0');
            RoundCounterxDN <= (others => '0');
          else
            RoundCounterxDN <= std_logic_vector(unsigned(RoundCounterxDP) + 1);
            StateMachinexDN <= std_logic_vector(to_unsigned(STATE_ROUND_OP, STATE_MACHINE_BITS));
          end if;
        end if;

        if (CP_CommandxSP = CP_CMD_FINAL_ENCRYPT) or
          (CP_CommandxSP = CP_CMD_FINAL_DECRYPT) or
          (CP_CommandxSP = CP_CMD_INIT) then
          if RoundCounterxDV = ROUNDS_A-1 then
            RoundCounterxDN <= (others => '0');
          else
            RoundCounterxDN <= std_logic_vector(unsigned(RoundCounterxDP) + 1);
            StateMachinexDN <= std_logic_vector(to_unsigned(STATE_ROUND_OP, STATE_MACHINE_BITS));
          end if;
        end if;
      end if;
    end if;
    

  end process ControlProc;

  -- purpose: Datapath of Ascon
  -- type   : combinational
  DatapathProc : process (DP_DestinationxS, DP_OpASelxS, DP_OpBSelxS,
                          DP_OperationxS, DataWritexDI, IODataxDP, KeyxDP,
                          RoundCounterxDP, State0xDP, State1xDP, State2xDP,
                          State3xDP, State4xDP, Temp0xDP, Temp1xDP) is
    variable OpAxDV : std_logic_vector(STATE_WORD_SIZE-1 downto 0);
    variable OpBxDV : std_logic_vector(STATE_WORD_SIZE-1 downto 0);
    variable ResxDV : std_logic_vector(STATE_WORD_SIZE-1 downto 0);
  begin  -- process DatapathProc

    IODataxDN <= IODataxDP;
    State0xDN <= State0xDP;
    State1xDN <= State1xDP;
    State2xDN <= State2xDP;
    State3xDN <= State3xDP;
    State4xDN <= State4xDP;
    Temp0xDN  <= Temp0xDP;
    Temp1xDN  <= Temp1xDP;

    OpAxDV := (others => '0');
    OpBxDV := (others => '0');
    ResxDV := (others => '0');

    case DP_OpASelxS is
      when DP_OPERAND_SEL_STATE0      => OpAxDV := State0xDP;
      when DP_OPERAND_SEL_STATE1      => OpAxDV := State1xDP;
      when DP_OPERAND_SEL_STATE2      => OpAxDV := State2xDP;
      when DP_OPERAND_SEL_STATE3      => OpAxDV := State3xDP;
      when DP_OPERAND_SEL_STATE4      => OpAxDV := State4xDP;
      when DP_OPERAND_SEL_KEY0        => OpAxDV := KeyxDP(63 downto 0);
      when DP_OPERAND_SEL_KEY1        => OpAxDV := KeyxDP(127 downto 64);
      when DP_OPERAND_SEL_CONST_INIT  => OpAxDV := CONST_KEY_SIZE & CONST_ROUNDS_A & CONST_ROUNDS_B & ZEROS(64-3*8);
      when DP_OPERAND_SEL_CONST_ROUND => OpAxDV := ZEROS(64-8) & not RoundCounterxDP(3 downto 0) & RoundCounterxDP(3 downto 0);
      when DP_OPERAND_SEL_IODATA      => OpAxDV := IODataxDP;
      when DP_OPERAND_SEL_TEMP0       => OpAxDV := Temp0xDP;
      when DP_OPERAND_SEL_TEMP1       => OpAxDV := Temp1xDP;
      when others                     => null;
    end case;

    case DP_OpBSelxS is
      when DP_OPERAND_SEL_STATE0      => OpBxDV := State0xDP;
      when DP_OPERAND_SEL_STATE1      => OpBxDV := State1xDP;
      when DP_OPERAND_SEL_STATE2      => OpBxDV := State2xDP;
      when DP_OPERAND_SEL_STATE3      => OpBxDV := State3xDP;
      when DP_OPERAND_SEL_STATE4      => OpBxDV := State4xDP;
      when DP_OPERAND_SEL_KEY0        => OpBxDV := KeyxDP(63 downto 0);
      when DP_OPERAND_SEL_KEY1        => OpBxDV := KeyxDP(127 downto 64);
      when DP_OPERAND_SEL_CONST_INIT  => OpBxDV := CONST_KEY_SIZE & CONST_ROUNDS_A & CONST_ROUNDS_B & ZEROS(64-3*8);
      when DP_OPERAND_SEL_CONST_ROUND => OpBxDV := ZEROS(64-8) & not RoundCounterxDP(3 downto 0) & RoundCounterxDP(3 downto 0);
      when DP_OPERAND_SEL_CONST_ONE   => OpBxDV := std_logic_vector(to_unsigned(1, STATE_WORD_SIZE));
      when DP_OPERAND_SEL_IODATA      => OpBxDV := IODataxDP;
      when DP_OPERAND_SEL_TEMP0       => OpBxDV := Temp0xDP;
      when DP_OPERAND_SEL_TEMP1       => OpBxDV := Temp1xDP;
      when others                     => null;
    end case;

    case DP_OperationxS is
      when DP_OPERATION_XOR      => ResxDV := OpAxDV xor OpBxDV;
      when DP_OPERATION_NOT_AND  => ResxDV := (not OpAxDV) and OpBxDV;
      when DP_OPERATION_NOT      => ResxDV := not OpAxDV;
      when DP_OPERATION_BUS_LOW  => ResxDV := OpAxDV(63 downto 32) & DataWritexDI;
      when DP_OPERATION_BUS_HIGH => ResxDV := DataWritexDI & OpAxDV(31 downto 0);
      when DP_OPERATION_ROT1     => ResxDV := ROTATE_STATE_WORD(OpAxDV, 1);
      when DP_OPERATION_ROT2     => ResxDV := ROTATE_STATE_WORD(OpAxDV, 2);
      when DP_OPERATION_ROT4     => ResxDV := ROTATE_STATE_WORD(OpAxDV, 4);
      when DP_OPERATION_ROT8     => ResxDV := ROTATE_STATE_WORD(OpAxDV, 8);
      when DP_OPERATION_ROT16    => ResxDV := ROTATE_STATE_WORD(OpAxDV, 16);
      when DP_OPERATION_ROT32    => ResxDV := ROTATE_STATE_WORD(OpAxDV, 32);
      when others                => null;
    end case;
    DP_ALU_ResultxD <= ResxDV;

    case DP_DestinationxS is
      when DP_DESTINATION_STATE0 => State0xDN <= ResxDV;
      when DP_DESTINATION_STATE1 => State1xDN <= ResxDV;
      when DP_DESTINATION_STATE2 => State2xDN <= ResxDV;
      when DP_DESTINATION_STATE3 => State3xDN <= ResxDV;
      when DP_DESTINATION_STATE4 => State4xDN <= ResxDV;
      when DP_DESTINATION_IODATA => IODataxDN <= ResxDV;
      when DP_DESTINATION_TEMP0  => Temp0xDN  <= ResxDV;
      when DP_DESTINATION_TEMP1  => Temp1xDN  <= ResxDV;
      when others                => null;
    end case;

  end process DatapathProc;
  

end architecture structural;
