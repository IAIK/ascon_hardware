-------------------------------------------------------------------------------
-- Title      : Ascon Counter
-- Project    : 
-------------------------------------------------------------------------------
-- File       : ascon_counter.vhdl
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

entity ascon_counter is

  port (
    ClkxCI                 : in  std_logic;
    RstxRBI                : in  std_logic;
    CountEnablexSI         : in std_logic;
    CounterRoundxDO        : out std_logic_vector(3 downto 0);
    CounterFunctSelxDO     : out std_logic_vector(2 downto 0);
    CounterSubIterationxDO : out std_logic_vector(5 downto 0));

end entity ascon_counter;

architecture structural of ascon_counter is
  signal CounterxDP : std_logic_vector(12 downto 0);     
begin  -- architecture structural

  counter_reg_p: process (ClkxCI, RstxRBI) is
  begin  -- process counter_reg_p
    if RstxRBI = '0' then               -- asynchronous reset (active low)
      CounterxDP <= (others => '0');
    elsif ClkxCI'event and ClkxCI = '1' then  -- rising clock edge
      if CountEnablexSI = '1' then -- Count enable
          CounterxDP <= std_logic_vector(unsigned(CounterxDP) + 1);
      end if;
    end if;
  end process counter_reg_p;

  CounterRoundxDO        <= CounterxDP(12 downto 9);
  CounterFunctSelxDO     <= CounterxDP( 8 downto 6);
  CounterSubIterationxDO <= CounterxDP( 5 downto 0);

end architecture structural;
