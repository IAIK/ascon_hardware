-------------------------------------------------------------------------------
-- Title      : Ascon
-- Project    : 
-------------------------------------------------------------------------------
-- File       : top.vhdl
-- Author     : Hannes Gross <hannes.gross@iaik.tugraz.at>
-- Company    : 
-- Created    : 2016-05-25
-- Last update: 2016-06-14
-- Platform   : 
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
-- Date        Version  Author       Description
-- 2016-05-25  1.0      Hannes Gross created
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity top is

  generic (
    -- Select Ascon variant:
    -- DATA_BUS_WIDTH =  64 ... Ascon-128 with 1,2,3 or 6 unrolled rounds
    -- DATA_BUS_WIDTH = 128 ... Ascon-128a with 1,2 or 4 untrolled rounds
    DATA_BUS_WIDTH  : integer := 128;
    UNROLED_ROUNDS  : integer := 4;
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

  -- Ascon 128 Variant
  ascon128_gen : if DATA_BUS_WIDTH = 64  generate
    ascon_1 : entity work.ascon
      generic map (
        UNROLED_ROUNDS  => UNROLED_ROUNDS, -- 1,2,3 or 6
        DATA_BLOCK_SIZE => DATA_BUS_WIDTH,
        ROUNDS_A        => 12,
        ROUNDS_B        => 6,
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
  end generate ascon128_gen;

    -- Ascon 128a Variant
  ascon128a_gen : if DATA_BUS_WIDTH = 128  generate
    ascon_1 : entity work.ascon
      generic map (
        UNROLED_ROUNDS  => UNROLED_ROUNDS, -- 1,2 or 4
        DATA_BLOCK_SIZE => DATA_BUS_WIDTH,
        ROUNDS_A        => 12,
        ROUNDS_B        => 8,
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
  end generate ascon128a_gen;

end architecture structural;
