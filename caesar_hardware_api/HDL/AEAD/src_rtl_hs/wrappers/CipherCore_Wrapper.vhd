-------------------------------------------------------------------------------
--! @file       CipherCore_Wrapper.vhd
--! @brief      5-bit Wrapper for CipherCore
--! @project    CAESAR Candidate Evaluation
--! @author     Ekawat (ice) Homsirikamol
--! @copyright  Copyright (c) 2015 Cryptographic Engineering Research Group
--!             ECE Department, George Mason University Fairfax, VA, U.S.A.
--!             All rights Reserved.
--! @license    This project is released under the GNU Public License.
--!             The license and distribution terms for this file may be
--!             found in the file LICENSE in this distribution or at
--!             http://www.gnu.org/licenses/gpl-3.0.txt
--! @note       This is publicly available encryption source code that falls
--!             under the License Exception TSU (Technology and software-
--!             —unrestricted)
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.ALL;
use work.AEAD_pkg.all;

entity CipherCore_Wrapper is
    generic (
        --! Reset behavior
        G_ASYNC_RSTN    : boolean := False; --! Async active low reset
        --! Block size (bits)
        G_DBLK_SIZE     : integer := 128;   --! Data
        G_KEY_SIZE      : integer := 128;   --! Key
        G_TAG_SIZE      : integer := 128    --! Tag
    );
    port (
        --! Global signals
        clk             : in  std_logic;
        rst             : in  std_logic;

        --! SERDES signals
        sin             : in  std_logic;
        ssel            : in  std_logic;
        sout            : out std_logic
    );
end entity CipherCore_Wrapper;

architecture structure of CipherCore_Wrapper is
    constant LBS_BYTES      : integer   := log2_ceil(G_DBLK_SIZE/8);
    
    signal sipo         : std_logic_vector(G_KEY_SIZE+G_DBLK_SIZE+G_DBLK_SIZE/4+LBS_BYTES+12 downto 0);
    signal piso         : std_logic_vector(G_DBLK_SIZE+LBS_BYTES+6    -1 downto 0);
    signal piso_data    : std_logic_vector(G_DBLK_SIZE+LBS_BYTES+6    -1 downto 0);
begin
    process(clk)
    begin
        if rising_edge(clk) then
            sipo <= sin & sipo(sipo'high downto 1);
            if (ssel = '1') then
                piso <= piso_data;
            else
                piso <= '0' & piso(piso'high downto 1);
            end if;
        end if;
    end process;
    sout <= piso(0);

    u_ciphercore:
    entity work.CipherCore(structure)
    generic map (
        G_ASYNC_RSTN        => G_ASYNC_RSTN     ,
        G_DBLK_SIZE         => G_DBLK_SIZE      ,
        G_KEY_SIZE          => G_KEY_SIZE       ,
        G_TAG_SIZE          => G_TAG_SIZE       ,
        G_LBS_BYTES         => LBS_BYTES
    )
    port map (
        clk                 => clk,
        rst                 => rst,
        --! Input
        key                 => sipo(G_KEY_SIZE                                              -1 downto 0                     ),
        bdi                 => sipo(G_KEY_SIZE+G_DBLK_SIZE                                  -1 downto G_KEY_SIZE            ),        
        key_valid           => sipo(G_KEY_SIZE+G_DBLK_SIZE+1                                -1),
        key_update          => sipo(G_KEY_SIZE+G_DBLK_SIZE+2                                -1),
        decrypt             => sipo(G_KEY_SIZE+G_DBLK_SIZE+3                                -1),
        bdo_ready           => sipo(G_KEY_SIZE+G_DBLK_SIZE+4                                -1),
        bdi_valid           => sipo(G_KEY_SIZE+G_DBLK_SIZE+5                                -1),
        bdi_type            => sipo(G_KEY_SIZE+G_DBLK_SIZE+8                                -1 downto G_KEY_SIZE+G_DBLK_SIZE+5),
        bdi_eot             => sipo(G_KEY_SIZE+G_DBLK_SIZE+9                                -1),
        bdi_eoi             => sipo(G_KEY_SIZE+G_DBLK_SIZE+10                               -1),                
        bdi_valid_bytes     => sipo(G_KEY_SIZE+G_DBLK_SIZE+10+G_DBLK_SIZE/8                 -1 downto G_KEY_SIZE+G_DBLK_SIZE+10),
        bdi_pad_loc         => sipo(G_KEY_SIZE+G_DBLK_SIZE+10+G_DBLK_SIZE/4                 -1 downto G_KEY_SIZE+G_DBLK_SIZE+10+G_DBLK_SIZE/8),
        bdi_size            => sipo(G_KEY_SIZE+G_DBLK_SIZE+10+G_DBLK_SIZE/4+LBS_BYTES+1     -1 downto G_KEY_SIZE+G_DBLK_SIZE+10+G_DBLK_SIZE/4),
        bdi_partial         => sipo(G_KEY_SIZE+G_DBLK_SIZE+10+G_DBLK_SIZE/4+LBS_BYTES+2     -1),
        
        --! Output
        bdo                 => piso_data(G_DBLK_SIZE                -1 downto 0),
        key_ready           => piso_data(G_DBLK_SIZE+1              -1),
        bdi_ready           => piso_data(G_DBLK_SIZE+2              -1),
        bdo_valid           => piso_data(G_DBLK_SIZE+3              -1),
        msg_auth_done       => piso_data(G_DBLK_SIZE+4              -1),
        msg_auth_valid      => piso_data(G_DBLK_SIZE+5              -1),
        bdo_size            => piso_data(G_DBLK_SIZE+5+LBS_BYTES+1  -1 downto G_DBLK_SIZE+5)
    );
end structure;