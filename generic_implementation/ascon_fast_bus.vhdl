-------------------------------------------------------------------------------
-- Title      : Bus logic of Ascon module
-- Project    : 
-------------------------------------------------------------------------------
-- File       : ascon_fast_bus.vhdl
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

entity ascon is
  
  generic (
    UNROLED_ROUNDS  : integer := 1; --1,2,3,6
    KEY_SIZE        : integer := 128;
    DATA_BLOCK_SIZE : integer := 64;
    ROUNDS_A        : integer := 12;
    ROUNDS_B        : integer := 6;
    DATA_BUS_WIDTH  : integer := 32;
    ADDR_BUS_WIDTH  : integer := 8);

  port (
    ClkxCI       : in  std_logic;
    RstxRBI      : in  std_logic;
    CSxSI        : in  std_logic;       -- active-high chip select
    WExSI        : in  std_logic;       -- active-high write enable
    AddressxDI   : in  std_logic_vector(ADDR_BUS_WIDTH-1 downto 0);
    DataWritexDI : in  std_logic_vector(DATA_BUS_WIDTH-1 downto 0);
    DataReadxDO  : out std_logic_vector(DATA_BUS_WIDTH-1 downto 0));

end entity ascon;

architecture structural of ascon is

  constant CONTROL_STATE_SIZE : integer := 4;
  constant STATE_WORD_SIZE    : integer := 64;

  constant CONST_UNROLED_R : std_logic_vector(7 downto 0) := std_logic_vector(to_unsigned(UNROLED_ROUNDS, 8));
  constant CONST_KEY_SIZE  : std_logic_vector(7 downto 0) := std_logic_vector(to_unsigned(KEY_SIZE, 8));
  constant CONST_ROUNDS_A  : std_logic_vector(7 downto 0) := std_logic_vector(to_unsigned(ROUNDS_A, 8));
  constant CONST_ROUNDS_B  : std_logic_vector(7 downto 0) := std_logic_vector(to_unsigned(ROUNDS_B, 8));

  signal CP_InitxSP, CP_InitxSn                         : std_logic;
  signal CP_AssociatexSP, CP_AssociatexSN               : std_logic;
  signal CP_EncryptxSP, CP_EncryptxSN                   : std_logic;
  signal CP_DecryptxSP, CP_DecryptxSN                   : std_logic;
  signal CP_FinalEncryptxSP, CP_FinalEncryptxSN         : std_logic;
  signal CP_FinalDecryptxSP, CP_FinalDecryptxSN         : std_logic;
  signal KeyxDP, KeyxDN                                 : std_logic_vector(KEY_SIZE-1 downto 0);

  signal DP_WriteNoncexS                                : std_logic;
  signal DP_WriteIODataxS                               : std_logic;
  signal CP_DonexS, CP_InitxS, CP_AssociatexSI          : std_logic;
  signal CP_EncryptxS, CP_DecryptxS, CP_FinalEncryptxS  : std_logic;
  signal CP_FinalDecryptxS                              : std_logic;
  signal IODataxD                                       : std_logic_vector(DATA_BLOCK_SIZE-1 downto 0);
  signal StatexD                                        : std_logic_vector(5*STATE_WORD_SIZE-1 downto 0);
  alias  State0xD                                       : std_logic_vector(STATE_WORD_SIZE-1 downto 0) is StatexD(64*1 -1 downto 64*0);
  alias  State1xD                                       : std_logic_vector(STATE_WORD_SIZE-1 downto 0) is StatexD(64*2 -1 downto 64*1);
  alias  State2xD                                       : std_logic_vector(STATE_WORD_SIZE-1 downto 0) is StatexD(64*3 -1 downto 64*2);
  alias  State3xD                                       : std_logic_vector(STATE_WORD_SIZE-1 downto 0) is StatexD(64*4 -1 downto 64*3);
  alias  State4xD                                       : std_logic_vector(STATE_WORD_SIZE-1 downto 0) is StatexD(64*5 -1 downto 64*4);

  function ZEROS (
    constant WIDTH : natural)
    return std_logic_vector is
    variable x : std_logic_vector(WIDTH-1 downto 0);
  begin  -- ZEROS
    x := (others => '0');
    return x;
  end ZEROS;

  function ROTATE_STATE_WORD (
    word            : std_logic_vector(STATE_WORD_SIZE-1 downto 0);
    constant rotate : integer)
    return std_logic_vector is
    variable x : std_logic_vector(STATE_WORD_SIZE-1 downto 0);
  begin  -- ROTATE_STATE_WORD
    x := word(ROTATE-1 downto 0) & word(STATE_WORD_SIZE-1 downto ROTATE);
    return x;
  end ROTATE_STATE_WORD;
  
begin  -- architecture structural


  -- purpose: Defines all registers
  -- type   : sequential
  -- inputs : ClkxCI, RstxRBI, *xDN signals
  -- outputs: *xDP signals
  RegisterProc : process (ClkxCI, RstxRBI) is
  begin  -- process RegisterProc
    if RstxRBI = '0' then               -- asynchronous reset (active low)
      KeyxDP                    <= (others => '0');
      CP_InitxSP                <= '0';
      CP_AssociatexSP           <= '0';
      CP_EncryptxSP             <= '0';
      CP_DecryptxSP             <= '0';
      CP_FinalEncryptxSP        <= '0';
      CP_FinalDecryptxSP        <= '0';
    elsif ClkxCI'event and ClkxCI = '1' then  -- rising clock edge
      KeyxDP                    <= KeyxDN;
      CP_InitxSP                <= CP_InitxSN;
      CP_AssociatexSP           <= CP_AssociatexSN;
      CP_EncryptxSP             <= CP_EncryptxSN;
      CP_DecryptxSP             <= CP_DecryptxSN;
      CP_FinalEncryptxSP        <= CP_FinalEncryptxSN;
      CP_FinalDecryptxSP        <= CP_FinalDecryptxSN;
    end if;
  end process RegisterProc;


  -- purpose: Glue the internal registers with the bus
  -- type   : combinational
  DataBusLogicProc : process (AddressxDI, CP_AssociatexSP,
                              CP_DecryptxSP, CP_DonexS, CP_EncryptxSP,
                              CP_FinalDecryptxSP, CP_FinalEncryptxSP,
                              CP_InitxSP, CSxSI, DataWritexDI, DataWritexDI(0),
                              DataWritexDI, IODataxD, KeyxDP,
                              State3xD,
                              State4xD,
                              WExSI) is
    variable AddressxDV : integer;
    variable index      : integer;
  begin  -- process DataBusLogicProc
    KeyxDN <= KeyxDP;

    AddressxDV  := to_integer(unsigned(AddressxDI));
    index       := 0;
    DataReadxDO <= (others => '0');

    DP_WriteNoncexS  <= '0';
    DP_WriteIODataxS <= '0';

    CP_InitxSN         <= CP_InitxSP;
    CP_AssociatexSN    <= CP_AssociatexSP;
    CP_EncryptxSN      <= CP_EncryptxSP;
    CP_DecryptxSN      <= CP_DecryptxSP;
    CP_FinalEncryptxSN <= CP_FinalEncryptxSP;
    CP_FinalDecryptxSN <= CP_FinalDecryptxSP;

    if CP_DonexS = '1' then
      CP_InitxSN         <= '0';
      CP_AssociatexSN    <= '0';
      CP_EncryptxSN      <= '0';
      CP_DecryptxSN      <= '0';
      CP_FinalEncryptxSN <= '0';
      CP_FinalDecryptxSN <= '0';
    end if;

    if CSxSI = '1' then
      if WExSI = '1' then
        -- synchronous write
        if AddressxDV = 2 then
          -- command register
          CP_InitxSN         <= DataWritexDI(0);
          CP_AssociatexSN    <= DataWritexDI(1);
          CP_EncryptxSN      <= DataWritexDI(2);
          CP_DecryptxSN      <= DataWritexDI(3);
          CP_FinalEncryptxSN <= DataWritexDI(4);
          CP_FinalDecryptxSN <= DataWritexDI(5);
        elsif (AddressxDV >= 4) and (AddressxDV < 8) then
          -- write the key
          index                                                          := to_integer(unsigned(AddressxDI(1 downto 0)));
          KeyxDN((index+1)*DATA_BUS_WIDTH-1 downto index*DATA_BUS_WIDTH) <= DataWritexDI;
        elsif (AddressxDV >= 8) and (AddressxDV < 12) then
          -- write the nonce
          DP_WriteNoncexS <= '1';
        --elsif (AddressxDV >= 12) and (AddressxDV < 14) then
        --  -- write the data to de/encrypt and associated data
        --  DP_WriteIODataxS <= '1';
        end if;
      else
        -- asynchronous read
        if AddressxDV = 0 then
          DataReadxDO <= x"deadbeef";
        elsif AddressxDV = 1 then
          -- status register
          -- returns 1 if busy
          DataReadxDO(0) <= CP_InitxSP or CP_AssociatexSP or CP_EncryptxSP or CP_DecryptxSP or CP_FinalEncryptxSP or CP_FinalDecryptxSP;
        elsif (AddressxDV >= 12) and (AddressxDV < 14) then
          -- read the de/encrypted data and associated data
          index       := to_integer(unsigned(AddressxDI(0 downto 0)));
          DataReadxDO <= IODataxD((index+1)*DATA_BUS_WIDTH-1 downto index*DATA_BUS_WIDTH);
        elsif (AddressxDV >= 16) and (AddressxDV < 20) then
          -- read the tag
          if DATA_BUS_WIDTH = 64 then
            if AddressxDI(1 downto 0) = "00" then
              DataReadxDO <= State4xD;
            elsif AddressxDI(1 downto 0) = "01" then
              DataReadxDO <= State3xD;
            end if;
          else -- 128 bit variant
              DataReadxDO <= State3xD & State4xD;
          end if;
        end if;
      end if;
    end if;
  end process DataBusLogicProc;


  ascon_core_1: entity work.ascon_core
    generic map (
      UNROLED_ROUNDS  => UNROLED_ROUNDS,
      KEY_SIZE        => KEY_SIZE,
      DATA_BLOCK_SIZE => DATA_BLOCK_SIZE,
      ROUNDS_A        => ROUNDS_A,
      ROUNDS_B        => ROUNDS_B,
      DATA_BUS_WIDTH  => DATA_BUS_WIDTH,
      ADDR_BUS_WIDTH  => ADDR_BUS_WIDTH)
    port map (
      ClkxCI                    => ClkxCI,
      RstxRBI                   => RstxRBI,
      AddressxDI                => AddressxDI,
      DP_WriteNoncexSI          => DP_WriteNoncexS,
      DataWritexDI              => DataWritexDI,
      KeyxDI                    => KeyxDP,
      DP_WriteIODataxSI         => DP_WriteIODataxS,
      IODataxDO                 => IODataxD,
      CP_DonexSO                => CP_DonexS,
      CP_InitxSI                => CP_InitxSP,
      CP_AssociatexSI           => CP_AssociatexSP,
      CP_EncryptxSI             => CP_EncryptxSP,
      CP_DecryptxSI             => CP_DecryptxSP,
      CP_FinalEncryptxSI        => CP_FinalEncryptxSP,
      CP_FinalDecryptxSI        => CP_FinalDecryptxSP,
      StatexDO                  => StatexD);

end architecture structural;
