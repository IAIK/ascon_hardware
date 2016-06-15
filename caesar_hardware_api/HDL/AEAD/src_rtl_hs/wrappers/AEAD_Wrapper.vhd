-------------------------------------------------------------------------------
--! @file       AEAD_Wrapper.vhd
--! @brief      5-bit wrapper for AEAD.vhd
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
use ieee.std_logic_1164.all;

entity AEAD_Wrapper is
    generic (
        G_W             : integer := 32;
        G_SW            : integer := 32
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
end entity AEAD_Wrapper;

architecture structure of AEAD_Wrapper is
    signal sipo         : std_logic_vector(G_W+G_SW+3       -1 downto 0);
    signal piso         : std_logic_vector(G_W+3            -1 downto 0);
    signal piso_data    : std_logic_vector(G_W+3            -1 downto 0);
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

    u_aead:
    entity work.AEAD(structure)
    generic map (
        G_W                     => G_W                          ,
        G_SW                    => G_SW
    )
    port map (
        clk                     => clk                          ,
        rst                     => rst                          ,
                
        --! Input signals
        pdi_data                => sipo(       G_W-1 downto   0),
        sdi_data                => sipo(  G_SW+G_W-1 downto G_W),
        pdi_valid               => sipo(1+G_SW+G_W-1)           ,
        sdi_valid               => sipo(2+G_SW+G_W-1)           ,
        do_ready                => sipo(3+G_SW+G_W-1)           ,
        
        --! Output signals
        do_data                 => piso_data(  G_W-1 downto   0),
        pdi_ready               => piso_data(1+G_W-1)           ,
        sdi_ready               => piso_data(2+G_W-1)           ,
        do_valid                => piso_data(3+G_W-1)
    );
end structure;