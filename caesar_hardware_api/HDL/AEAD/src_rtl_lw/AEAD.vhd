-------------------------------------------------------------------------------
--! @file       AEAD.vhd
--! @brief      Top-level template of authenticated encryption unit.
--!
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

entity AEAD is
    generic (
        --! I/O size (bits)
        G_W             : integer := 32;    --! Public data input
        G_SW            : integer := 32     --! Secret data input        
    );
    port (
        --! Global ports
        clk             : in  std_logic;
        rst             : in  std_logic;
        --! Publica data ports
        pdi             : in  std_logic_vector(G_W              -1 downto 0);
        pdi_valid       : in  std_logic;
        pdi_ready       : out std_logic;
        --! Secret data ports
        sdi             : in  std_logic_vector(G_SW             -1 downto 0);
        sdi_valid       : in  std_logic;
        sdi_ready       : out std_logic;
        --! Data out ports
        do              : out std_logic_vector(G_W              -1 downto 0);
        do_ready        : in  std_logic;
        do_valid        : out std_logic
    );
end AEAD;

-------------------------------------------------------------------------------
--! @brief  Architecture definition of AEAD
-------------------------------------------------------------------------------
architecture structure of AEAD is
begin
end architecture structure;