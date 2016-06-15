-------------------------------------------------------------------------------
-- Title      : A super fast implementation of Ascon
-- Project    : Ascon
-------------------------------------------------------------------------------
-- File       : ascon_fast.vhdl
-- Author     : Erich Wenger  <erich.wenger@iaik.tugraz.at>
-- Company    : Graz University of Technology
-- Created    : 2014-03-21
-- Last update: 2014-09-22
-- Platform   : ASIC design
-- Standard   : VHDL'93/02
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

    DataInxDI    : in  std_logic_vector(DATA_BLOCK_SIZE-1 downto 0);
    DataOutxDO   : out std_logic_vector(DATA_BLOCK_SIZE-1 downto 0));

end entity ascon;

architecture structural of ascon is

  constant CONTROL_STATE_SIZE : integer := 4;
  constant STATE_WORD_SIZE    : integer := 64;

  constant CONST_KEY_SIZE : std_logic_vector(7 downto 0) := std_logic_vector(to_unsigned(KEY_SIZE, 8));
  constant CONST_ROUNDS_A : std_logic_vector(7 downto 0) := std_logic_vector(to_unsigned(ROUNDS_A, 8));
  constant CONST_ROUNDS_B : std_logic_vector(7 downto 0) := std_logic_vector(to_unsigned(ROUNDS_B, 8));

  type state_type is array (4 downto 0) of std_logic_vector(STATE_WORD_SIZE-1 downto 0);
  
  signal StatexDP, StatexDN             : state_type;

  signal DataInxDP, DataInxDN : std_logic_vector(DATA_BLOCK_SIZE-1 downto 0);
  signal DataOutxDP, DataOutxDN : std_logic_vector(DATA_BLOCK_SIZE-1 downto 0);

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

  function RoundFunction (
    constant roundconst : std_logic_vector(3 downto 0);
    StateInxD    : state_type)
    return state_type is
    variable P0xDV, P1xDV, P2xDV, P3xDV, P4xDV : std_logic_vector(STATE_WORD_SIZE-1 downto 0);
    variable R0xDV, R1xDV, R2xDV, R3xDV, R4xDV : std_logic_vector(STATE_WORD_SIZE-1 downto 0);
    variable S0xDV, S1xDV, S2xDV, S3xDV, S4xDV : std_logic_vector(STATE_WORD_SIZE-1 downto 0);
    variable T0xDV, T1xDV, T2xDV, T3xDV, T4xDV : std_logic_vector(STATE_WORD_SIZE-1 downto 0);
    variable U0xDV, U1xDV, U2xDV, U3xDV, U4xDV : std_logic_vector(STATE_WORD_SIZE-1 downto 0);
    variable RoundConstxDV                     : std_logic_vector(63 downto 0);
    variable StateOutxD : state_type;
  begin  -- function RoundFunction
    RoundConstxDV      := ZEROS(64-8) & not roundconst(3 downto 0) & roundconst(3 downto 0);

    P0xDV := StateInxD(0);
    P1xDV := StateInxD(1);
    P2xDV := StateInxD(2);
    P3xDV := StateInxD(3);
    P4xDV := StateInxD(4);

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

    StateOutxD(0) := U0xDV;
    StateOutxD(1) := U1xDV;
    StateOutxD(2) := U2xDV;
    StateOutxD(3) := U3xDV;
    StateOutxD(4) := U4xDV;
    return StateOutxD;
  end function RoundFunction;
  
begin  -- architecture structural

  -- purpose: Defines all registers
  -- type   : sequential
  -- inputs : ClkxCI, RstxRBI, *xDN signals
  -- outputs: *xDP signals
  RegisterProc : process (ClkxCI, RstxRBI) is
  begin  -- process RegisterProc
    if RstxRBI = '0' then                     -- asynchronous reset (active low)
      StatexDP                 <= (others => (others => '0'));
      DataInxDP <= (others => '0');
      DataOutxDP <= (others => '0');
    elsif ClkxCI'event and ClkxCI = '1' then  -- rising clock edge
      StatexDP                 <= StatexDN;
      DataInxDP                 <= DataInxDN;
      DataOutxDP                <= DataOutxDN;
    end if;
  end process RegisterProc;

  -- purpose: Datapath of Ascon
  -- type   : combinational
  DatapathProc : process (DataInxDI, DataInxDP, DataOutxDP, StatexDP) is
    variable State0xD, State1xD, State2xD, State3xD, State4xD, State5xD, State6xD : state_type;
    variable P0xD : std_logic_vector(63 downto 0);
  begin  -- process DatapathProc
    DataInxDN <= DataInxDI;
    DataOutxDO <= DataOutxDP;
    
    State0xD := StatexDP;
    P0xD := StatexDP(0) xor DataInxDP;
    DataOutxDN <= P0xD;
    State0xD(0) := P0xD;

    State1xD := RoundFunction("0000", State0xD);
    State2xD := RoundFunction("0001", State1xD);
    State3xD := RoundFunction("0010", State2xD);
    State4xD := RoundFunction("0011", State3xD);
    State5xD := RoundFunction("0100", State4xD);
    State6xD := RoundFunction("0101", State5xD);

    StatexDN <= State6xD;
  end process DatapathProc;


end architecture structural;
