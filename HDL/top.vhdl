-------------------------------------------------------------------------------
-- Title      : Top module that instantiates ascon
-- Project    : 
-------------------------------------------------------------------------------
-- File       : top.vhdl
-- Author     : Erich Wenger  <erichwenger@erich.wenger@iaik.tugraz.at>
-- Company    : 
-- Created    : 2014-03-24
-- Last update: 2014-03-24
-- Platform   : 
-- Standard   : VHDL'93/02
-------------------------------------------------------------------------------
-- Description: 
-------------------------------------------------------------------------------
-- Copyright (c) 2014 
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
