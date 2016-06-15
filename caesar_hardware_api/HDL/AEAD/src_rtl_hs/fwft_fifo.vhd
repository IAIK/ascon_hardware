-------------------------------------------------------------------------------
--! @file       fwft_fifo.vhd
--! @brief      First-Word-Fall-Through FIFO
--! @project    CAESAR Candidate Evaluation
--! @author     Ekawat (ice) Homsirikamol
--! @copyright  Copyright (c) 2016 Cryptographic Engineering Research Group
--!             ECE Department, George Mason University Fairfax, VA, U.S.A.
--!             All rights Reserved.
--! @license    This project is released under the GNU Public License.
--!             The license and distribution terms for this file may be
--!             found in the file LICENSE in this distribution or at
--!             http://www.gnu.org/licenses/gpl-3.0.txt
--! @note       This is publicly available encryption source code that falls
--!             under the License Exception TSU (Technology and software-
--!             —unrestricted)
--! @ref        This code is based on the fwft_fifo by
--!             http://www.deathbylogic.com/2015/01/vhdl-first-word-fall-through-fifo/      
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity fwft_fifo is
    generic (
        G_W             : integer := 64;    --! Width of I/O (bits)
        G_LOG2DEPTH     : integer := 9;     --! LOG(2) of depth
        G_ASYNC_RSTN    : boolean := False  --! Async reset active low
    );
    port (
        clk             : in  std_logic;
        rst             : in  std_logic;
        din             : in  std_logic_vector(G_W              -1 downto 0);
        din_valid       : in  std_logic;
        din_ready       : out std_logic;
        dout            : out std_logic_vector(G_W              -1 downto 0);
        dout_valid      : out std_logic;
        dout_ready      : in  std_logic
    );
end fwft_fifo;

architecture structure of fwft_fifo is
    type t_mem is array (2**G_LOG2DEPTH-1 downto 0)
        of std_logic_vector(G_W-1 downto 0);
    signal iready : std_logic;
    signal ovalid : std_logic;
begin
    din_ready  <= iready;
    dout_valid <= ovalid;
    
    gSync:
    if (not G_ASYNC_RSTN) generate
        process(clk)
            variable memory : t_mem;
            variable wrptr  : std_logic_vector(G_LOG2DEPTH      -1 downto 0);
            variable rdptr  : std_logic_vector(G_LOG2DEPTH      -1 downto 0);
            variable looped : boolean;
        begin
            if rising_edge(clk) then
                if (rst = '1') then
                    wrptr  := (others => '0');
                    rdptr  := (others => '0');
                    looped := False;
                    
                    iready <= '1';
                    ovalid <= '0';
                else                                
                    if (dout_ready = '1' and ovalid = '1') then
                        if ((looped = True) or (wrptr /= rdptr)) then
                            if (unsigned(rdptr) = 2**G_LOG2DEPTH-1) then
                                looped := False;
                            end if;                    
                            rdptr := std_logic_vector(unsigned(rdptr) + 1);
                        end if;
                    end if;
                    
                    if (din_valid = '1' and iready = '1') then
                        if ((looped = False) or (wrptr /= rdptr)) then
                            memory(to_integer(unsigned(wrptr))) := din;
                            
                            if (unsigned(wrptr) = 2**G_LOG2DEPTH-1) then
                                looped := True;
                            end if;
                            wrptr := std_logic_vector(unsigned(wrptr) + 1);                        
                        end if;                    
                    end if;
                                                                   
                    dout <= memory(to_integer(unsigned(rdptr)));
                    
                    --! Update flags
                    if (wrptr = rdptr) then
                        if (looped) then
                            iready <= '0';
                        else
                            ovalid <= '0';
                        end if;
                    else
                        iready <= '1';
                        ovalid <= '1';
                    end if;
                end if;
            end if;
        end process;    
    end generate;
    gAsync:
    if (G_ASYNC_RSTN) generate
        process(clk, rst)
            variable memory : t_mem;
            variable wrptr  : std_logic_vector(G_LOG2DEPTH      -1 downto 0);
            variable rdptr  : std_logic_vector(G_LOG2DEPTH      -1 downto 0);
            variable looped : boolean;
        begin
            if (rst = '0') then
                wrptr  := (others => '0');
                rdptr  := (others => '0');
                looped := False;
                
                iready <= '1';
                ovalid <= '0';
            elsif rising_edge(clk) then
                if (dout_ready = '1' and ovalid = '1') then
                    if ((looped = True) or (wrptr /= rdptr)) then
                        if (unsigned(rdptr) = 2**G_LOG2DEPTH-1) then
                            looped := False;
                        end if;                    
                        rdptr := std_logic_vector(unsigned(rdptr) + 1);
                    end if;
                end if;
                
                if (din_valid = '1' and iready = '1') then
                    if ((looped = False) or (wrptr /= rdptr)) then
                        memory(to_integer(unsigned(wrptr))) := din;
                        
                        if (unsigned(wrptr) = 2**G_LOG2DEPTH-1) then
                            looped := True;
                        end if;
                        wrptr := std_logic_vector(unsigned(wrptr) + 1);                        
                    end if;                    
                end if;
                                                               
                dout <= memory(to_integer(unsigned(rdptr)));
                
                --! Update flags
                if (wrptr = rdptr) then
                    if (looped) then
                        iready <= '0';
                    else
                        ovalid <= '0';
                    end if;
                else
                    iready <= '1';
                    ovalid <= '1';
                end if;
            end if;
        end process;    
    end generate;
end architecture structure;
