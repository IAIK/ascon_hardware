-------------------------------------------------------------------------------
--! @file       AEAD_pkg.vhd
--! @brief      Package used for authenticated encyryption
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

package AEAD_pkg is
    --! =======================================================================
    --! BDI Type Encoding for CipherCore (based on SegmentType(3 downto 1))
    constant BDI_TYPE_ASS   : std_logic_vector(3    -1 downto 1) := "00";
    constant BDI_TYPE_ASS0  : std_logic_vector(3    -1 downto 0) := "000";
    constant BDI_TYPE_ASS1  : std_logic_vector(3    -1 downto 0) := "001";
    constant BDI_TYPE_DAT   : std_logic_vector(3    -1 downto 1) := "01";
    constant BDI_TYPE_DAT0  : std_logic_vector(3    -1 downto 0) := "010";
    constant BDI_TYPE_DAT1  : std_logic_vector(3    -1 downto 0) := "011";
    constant BDI_TYPE_TAG   : std_logic_vector(3    -1 downto 0) := "100";
    constant BDI_TYPE_LEN   : std_logic_vector(3    -1 downto 0) := "101";
    constant BDI_TYPE_NPUB  : std_logic_vector(3    -1 downto 0) := "110";
    constant BDI_TYPE_NSEC  : std_logic_vector(3    -1 downto 0) := "111";
    --! =======================================================================
    --! Opcode (used by Pre- and Post-Processors)
	constant OP_ENCDEC      : std_logic_vector(3    -1 downto 0) := "001";
    constant OP_ENC         : std_logic_vector(4    -1 downto 0) := "0010";
    constant OP_DEC         : std_logic_vector(4    -1 downto 0) := "0011";
    constant OP_LDKEY       : std_logic_vector(4    -1 downto 0) := "0100";
    constant OP_ACTKEY      : std_logic_vector(4    -1 downto 0) := "0111";
    --! =======================================================================
    --! Status (used by Pre- and Post-Processors)
    constant STAT_SUCCESS   : std_logic_vector(4    -1 downto 0) := "1110";
    constant STAT_FAILURE   : std_logic_vector(4    -1 downto 0) := "1111";
    --! =======================================================================
    --! Segment Type Encoding (used by Pre- and Post-Processors)
    --! 00XX
    constant ST_A           : std_logic_vector(2    -1 downto 0) := "00";
    constant ST_AD          : std_logic_vector(4    -1 downto 0) := "0001";
    constant ST_NPUB_AD     : std_logic_vector(4    -1 downto 0) := "0010";
    constant ST_AD_NPUB     : std_logic_vector(4    -1 downto 0) := "0011";
    --! 01XX
    constant ST_D           : std_logic_vector(2    -1 downto 0) := "01";
    constant ST_PT          : std_logic_vector(4    -1 downto 0) := "0100";
    constant ST_CT          : std_logic_vector(4    -1 downto 0) := "0101";
    constant ST_CT_TAG      : std_logic_vector(4    -1 downto 0) := "0110";
    --! 10XX
    constant ST_TAG         : std_logic_vector(4    -1 downto 0) := "1000";
    constant ST_LEN         : std_logic_vector(4    -1 downto 0) := "1010";
    --! 11XX
    constant ST_KEY         : std_logic_vector(4    -1 downto 0) := "1100";
    constant ST_NPUB        : std_logic_vector(4    -1 downto 0) := "1101";
    constant ST_NSEC        : std_logic_vector(3    -1 downto 0) := "111";
    constant ST_NSEC_PT     : std_logic_vector(4    -1 downto 0) := "1110";
    constant ST_NSEC_CT     : std_logic_vector(4    -1 downto 0) := "1111";
    --! =======================================================================
    --! Maximum supported length
    constant SINGLE_PASS_MAX  : integer := 32;
    constant TWO_PASS_MAX     : integer := 11;
    --! =======================================================================
    --! Functions
    function log2_ceil (N: natural) return natural;                                         --! Log(2) ceil
end AEAD_pkg;

package body AEAD_pkg is
    --! Log of base 2
    function log2_ceil (N: natural) return natural is
	begin
		 if ( N = 0 ) then
			 return 0;
		 elsif N <= 2 then
			 return 1;
		 else
			if (N mod 2 = 0) then
				return 1 + log2_ceil(N/2);
			else
				return 1 + log2_ceil((N+1)/2);
			end if;
		 end if;
	end function log2_ceil;
end package body AEAD_pkg;
