-------------------------------------------------------------------------------
-- Title      : Ascon Shift Register
-- Project    : 
-------------------------------------------------------------------------------
-- File       : ascon_shift_register.vhdl
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

entity ascon_shift_register is

  generic (
    RESET_VALUE           : std_logic_vector(63 downto 0) := x"0000000000000000";
    DATA_WIDTH            : integer := 64);
  
  port (
    ClkxCI                 : in  std_logic;
    RstxRBI                : in  std_logic;
    ShiftEnablexSI         : in  std_logic;
    ShiftRegINxDI          : in  std_logic;
    ShiftRegOUTxDO         : out std_logic_vector(DATA_WIDTH-1 downto 0));

end entity ascon_shift_register;

architecture structural of ascon_shift_register is
  signal DataxDP : std_logic_vector(DATA_WIDTH-1 downto 0);
begin  -- architecture structural

  ShiftRegOUTxDO <= DataxDP;
  
  -- purpose: Left shift each cycle
  -- type   : sequential
  -- inputs : ClkxCI, RstxRBI
  -- outputs: DataOUTxDO
  shift_p: process (ClkxCI, RstxRBI) is
  begin  -- process shift_p
    if RstxRBI = '0' then               -- asynchronous reset (active low)
      DataxDP <= RESET_VALUE;
    elsif ClkxCI'event and ClkxCI = '1' then  -- rising clock edge
     if ShiftEnablexSI = '1' then
        DataxDP <= DataxDP(DATA_WIDTH-2 downto 0) & ShiftRegINxDI; -- shift left
     end if;
    end if;
  end process shift_p;


end architecture structural;
