-------------------------------------------------------------------------------
--! @file       AEAD.vhd
--! @author     Hannes Gross
--! @brief      Generic Ascon-128(a) implementation
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use work.AEAD_pkg.all;

entity AEAD is
    generic (
        --! I/O width (bits)
        G_W             : integer := 32;    --! Public data input
        G_SW            : integer := 32;    --! Secret data input
        --! Reset behavior
        G_ASYNC_RSTN    : boolean := False; --! Async active low reset
        --! Special features parameters
        G_ENABLE_PAD    : boolean := True;  --! Enable padding
        G_CIPH_EXP      : boolean := False; --! Ciphertext expansion
        G_REVERSE_CIPH  : boolean := False; --! Reversed ciphertext
        G_MERGE_TAG     : boolean := False; --! Merge tag with data segment
        --! Block size (bits)
        G_ABLK_SIZE     : integer := 128;    --! Associated data 128 or 64
        G_DBLK_SIZE     : integer := 128;    --! Data 128 or 64
        G_KEY_SIZE      : integer := 32;    --! Key
        G_TAG_SIZE      : integer := 128;   --! Tag
        --! Padding options
        G_PAD_STYLE     : integer := 1;     --! Pad style
        G_PAD_AD        : integer := 3;     --! Padding behavior for AD
        G_PAD_D         : integer := 4;     --! Padding behavior for Data
        --! Maximum supported AD/message/ciphertext length = 2^G_MAX_LEN-1
        G_MAX_LEN       : integer := SINGLE_PASS_MAX
    );
    port (
        --! Global ports
        clk             : in  std_logic;
        rst             : in  std_logic;
        --! Publica data ports
        pdi_data        : in  std_logic_vector(G_W              -1 downto 0);
        pdi_valid       : in  std_logic;
        pdi_ready       : out std_logic;
        --! Secret data ports
        sdi_data        : in  std_logic_vector(G_SW             -1 downto 0);
        sdi_valid       : in  std_logic;
        sdi_ready       : out std_logic;
        --! Data out ports
        do_data         : out std_logic_vector(G_W              -1 downto 0);
        do_ready        : in  std_logic;
        do_valid        : out std_logic
    );
end AEAD;
