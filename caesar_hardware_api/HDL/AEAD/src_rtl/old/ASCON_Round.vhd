-------------------------------------------------------------------------------
--! @file       ASCON_Round.vhd
--! @author     Ekawat (ice) Homsirikamol
--! @brief      Round unit for ASCON
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity ASCON_Round is    
    port    ( 
        ii    : in  std_logic_vector(320                  -1 downto 0);
        rc    : in  std_logic_vector(  8                  -1 downto 0);
        oo    : out std_logic_vector(320                  -1 downto 0)
    );
end entity ASCON_Round;

architecture structure of ASCON_Round is   
    type stype is array ( 0 to     4) 
        of std_logic_vector(64    -1 downto 0);
    signal x        : stype;
    signal add      : stype; 
    signal sub      : stype;
    signal ldiff    : stype;
    
    type sboxtype is array (63 downto 0) 
        of std_logic_vector(5         -1 downto 0);
    signal subi     : sboxtype;
    signal subo     : sboxtype;
    
    type sbox_rom_type  is array ( 0 to    31) of integer range 0 to 31;
    constant sbox_rom : sbox_rom_type := 
        (4, 11, 31, 20, 26, 21, 9, 2, 27, 5, 8, 18, 29, 3, 6, 28,
         30, 19, 7, 14, 0, 13, 17, 24, 16, 12, 1, 25, 22, 10, 15, 23);
begin
    gmap:
    for i in 0 to 4 generate
        x(i) <= ii(320-1-64*i downto 320-64-64*i);
        
        oo(320-1-64*i downto 320-64-64*i) <= ldiff(i);
    end generate;
    
    --! Addition of RC
    add(0) <= x(0);
    add(1) <= x(1);
    add(2) <= x(2)(63 downto 8) & (x(2)(7 downto 0) xor rc);
    add(3) <= x(3);
    add(4) <= x(4);
    
    --! Substitution layer 
    gSub0:
    for i in 63 downto 0 generate
        subi(i)     <= add(0)(i) & add(1)(i) & add(2)(i) & add(3)(i) & add(4)(i);
        subo(i)     <= std_logic_vector(to_unsigned(sbox_rom(to_integer(unsigned(subi(i)))), 5));
        
        sub(0)(i)   <= subo(i)(4);
        sub(1)(i)   <= subo(i)(3);
        sub(2)(i)   <= subo(i)(2);
        sub(3)(i)   <= subo(i)(1);
        sub(4)(i)   <= subo(i)(0);
    end generate;
  
    --! Linear diffusion
    ldiff(0) <= sub(0) xor std_logic_vector(ROTATE_RIGHT(unsigned(sub(0)), 19)) xor std_logic_vector(ROTATE_RIGHT(unsigned(sub(0)), 28));
    ldiff(1) <= sub(1) xor std_logic_vector(ROTATE_RIGHT(unsigned(sub(1)), 61)) xor std_logic_vector(ROTATE_RIGHT(unsigned(sub(1)), 39));
    ldiff(2) <= sub(2) xor std_logic_vector(ROTATE_RIGHT(unsigned(sub(2)),  1)) xor std_logic_vector(ROTATE_RIGHT(unsigned(sub(2)),  6));
    ldiff(3) <= sub(3) xor std_logic_vector(ROTATE_RIGHT(unsigned(sub(3)), 10)) xor std_logic_vector(ROTATE_RIGHT(unsigned(sub(3)), 17));
    ldiff(4) <= sub(4) xor std_logic_vector(ROTATE_RIGHT(unsigned(sub(4)),  7)) xor std_logic_vector(ROTATE_RIGHT(unsigned(sub(4)), 41));    
end architecture structure;