library ieee;
use ieee.std_logic_1164.ALL;
use ieee.numeric_std.all;
use ieee.std_logic_textio.all;
use work.std_logic_1164_additions.all;
use work.AEAD_pkg.all;

library std;
use std.textio.all;

entity top is
  generic (
      G_W            : integer := 32;
      G_SW           : integer := 32;
      G_ASYNC_RSTN   : boolean := False;
      G_ENABLE_PAD   : boolean := True;
      G_CIPH_EXP     : boolean := False;
      G_REVERSE_CIPH : boolean := False;
      G_MERGE_TAG    : boolean := False;
      G_ABLK_SIZE    : integer := 64; -- change this when changing Ascon version
      G_DBLK_SIZE    : integer := 64;
      G_KEY_SIZE     : integer := 32;
      G_TAG_SIZE     : integer := 128;
      G_PAD_STYLE    : integer := 1;
      G_PAD_AD       : integer := 3;
      G_PAD_D        : integer := 4);

    port (
      clk       : in  std_logic;
      rst       : in  std_logic;
      pdi_data  : in  std_logic_vector(G_W -1 downto 0);
      pdi_valid : in  std_logic;
      pdi_ready : out std_logic;
      sdi_data  : in  std_logic_vector(G_SW -1 downto 0);
      sdi_valid : in  std_logic;
      sdi_ready : out std_logic;
      do_data   : out std_logic_vector(G_W -1 downto 0);
      do_ready  : in  std_logic;
      do_valid  : out std_logic);
end entity top;

architecture structural of top is

begin  -- architecture structural

  AEAD_1: entity work.AEAD
    generic map (
      G_W            => G_W,
      G_SW           => G_SW,
      G_ASYNC_RSTN   => G_ASYNC_RSTN,
      G_ENABLE_PAD   => G_ENABLE_PAD,
      G_CIPH_EXP     => G_CIPH_EXP,
      G_REVERSE_CIPH => G_REVERSE_CIPH,
      G_MERGE_TAG    => G_MERGE_TAG,
      G_ABLK_SIZE    => G_ABLK_SIZE,
      G_DBLK_SIZE    => G_DBLK_SIZE,
      G_KEY_SIZE     => G_KEY_SIZE,
      G_TAG_SIZE     => G_TAG_SIZE,
      G_PAD_STYLE    => G_PAD_STYLE,
      G_PAD_AD       => G_PAD_AD,
      G_PAD_D        => G_PAD_D)
    port map (
      clk       => clk,
      rst       => rst,
      pdi_data  => pdi_data,
      pdi_valid => pdi_valid,
      pdi_ready => pdi_ready,
      sdi_data  => sdi_data,
      sdi_valid => sdi_valid,
      sdi_ready => sdi_ready,
      do_data   => do_data,
      do_ready  => do_ready,
      do_valid  => do_valid);

end architecture structural;
