-------------------------------------------------------------------------------
-- Title      : Ascon SBox
-- Project    : 
-------------------------------------------------------------------------------
-- File       : ascon_sbox5.vhdl
-- Author     : Hannes Gross  <hannes.gross@iaik.tugraz.at>
-- Company    : 
-- Created    : 2014-05-20
-- Last update: 2014-05-23
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

entity ascon_sbox5 is

  port (
    SboxINxDI    : in  std_logic_vector(4 downto 0);
    SboxOUTxDO   : out std_logic_vector(4 downto 0));

end entity ascon_sbox5;

architecture structural of ascon_sbox5 is
      
begin  -- architecture structural

  -- purpose: implementation of the Ascno sbox
  -- type   : combinational
  sbox: process (SBoxINxDI) is

      -- Temp variables;
  variable SBoxT0xV, SBoxT1xV, SBoxT2xV : std_logic_vector(4 downto 0);
  
  begin  -- process sbox
    SBoxT0xV(0) := SBoxINxDI(0) xor SBoxINxDI(4);
    SBoxT0xV(1) := SBoxINxDI(1);
    SBoxT0xV(2) := SBoxINxDI(2) xor SBoxINxDI(1);
    SBoxT0xV(3) := SBoxINxDI(3);
    SBoxT0xV(4) := SBoxINxDI(4) xor SBoxINxDI(3);

    SBoxT1xV(0) := SBoxT0xV(0) xor (not SBoxT0xV(1) and SBoxT0xV(2));
    SBoxT1xV(1) := SBoxT0xV(1) xor (not SBoxT0xV(2) and SBoxT0xV(3));
    SBoxT1xV(2) := SBoxT0xV(2) xor (not SBoxT0xV(3) and SBoxT0xV(4));
    SBoxT1xV(3) := SBoxT0xV(3) xor (not SBoxT0xV(4) and SBoxT0xV(0));
    SBoxT1xV(4) := SBoxT0xV(4) xor (not SBoxT0xV(0) and SBoxT0xV(1));

    SboxOUTxDO(0) <= SBoxT1xV(0) xor SBoxT1xV(4);
    SboxOUTxDO(1) <= SBoxT1xV(1) xor SBoxT1xV(0);
    SboxOUTxDO(2) <= not SBoxT1xV(2);
    SboxOUTxDO(3) <= SBoxT1xV(3) xor SBoxT1xV(2);
    SboxOUTxDO(4) <= SBoxT1xV(4);
  end process sbox;

end architecture structural;
