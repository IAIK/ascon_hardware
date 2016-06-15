-------------------------------------------------------------------------------
-- Title      : Top module that instantiates ascon
-- Project    : 
-------------------------------------------------------------------------------
-- File       : top.vhdl
-- Author     : Erich Wenger  <erich.wenger@iaik.tugraz.at>
-- Company    : Graz University of Technology
-- Created    : 2014-03-24
-- Last update: 2014-03-24
-- Platform   : ASIC design
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
-- Date        Version  Author  Description
-- 2014-03-24  1.0      erichwenger	Created
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity top is

  generic (
    DATA_BUS_WIDTH  : integer := 32;
    ADDR_BUS_WIDTH  : integer := 8);

  port (
    ClkxCI       : in  std_logic;
    RstxRBI      : in  std_logic;
    CSxSI        : in  std_logic;
    WExSI        : in  std_logic;
    AddressxDI   : in  std_logic_vector(ADDR_BUS_WIDTH-1 downto 0);
    DataWritexDI : in  std_logic_vector(DATA_BUS_WIDTH-1 downto 0);
    DataReadxDO  : out std_logic_vector(DATA_BUS_WIDTH-1 downto 0));

end entity top;

architecture structural of top is

begin  -- architecture structural

  ascon_1: entity work.ascon
    generic map (
      DATA_BUS_WIDTH  => DATA_BUS_WIDTH,
      ADDR_BUS_WIDTH  => ADDR_BUS_WIDTH)
    port map (
      ClkxCI       => ClkxCI,
      RstxRBI      => RstxRBI,
      CSxSI        => CSxSI,
      WExSI        => WExSI,
      AddressxDI   => AddressxDI,
      DataWritexDI => DataWritexDI,
      DataReadxDO  => DataReadxDO);

end architecture structural;
