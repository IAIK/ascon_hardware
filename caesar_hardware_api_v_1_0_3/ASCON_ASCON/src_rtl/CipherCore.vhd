-------------------------------------------------------------------------------
--! @file       CipherCore.vhd
--! @author     Hannes Gross
--! @brief      Generic Ascon-128(a) implementation
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.ALL;
use ieee.numeric_std.all;
use work.AEAD_pkg.all;

entity CipherCore is
  generic (
    -- Ascon related generics
    RATE           : integer := 64;    -- Selects:
                                        -- (64) ->  Ascon128
                                        -- (128)->  Acon128a
    UNROLED_ROUNDS : integer := 1;      -- Ascon128: 1, 2, 3, or 6 rounds
                                        -- Ascon128a:   1, 2, or 4 rounds
    ROUNDS_A       : integer := 12;     -- Number of rounds for initialization
                                        -- and finalization
    ROUNDS_B       : integer := 6;      -- Num permutation rounds for data for
                                        -- (6) -> Ascon128
                                        -- (8) -> Ascon128a
    
    --- Interface generics:
    -- Reset behavior
    G_ASYNC_RSTN   : boolean := false;  --! Async active low reset
    -- Block size (bits)
    G_DBLK_SIZE    : integer := 128;     --! Data
    G_KEY_SIZE     : integer := 32;     --! Key
    G_TAG_SIZE     : integer := 128;    --! Tag
    -- The number of bits required to hold block size expressed in
    -- bytes = log2_ceil(G_DBLK_SIZE/8)
    G_LBS_BYTES    : integer := 4;
    G_MAX_LEN      : integer := SINGLE_PASS_MAX
    );
    port (
        --! Global
        clk             : in  std_logic;
        rst             : in  std_logic;
        --! PreProcessor (data)
        key             : in  std_logic_vector(G_KEY_SIZE       -1 downto 0);
        bdi             : in  std_logic_vector(G_DBLK_SIZE      -1 downto 0);
        --! PreProcessor (controls)
        key_ready       : out std_logic;
        key_valid       : in  std_logic;
        key_update      : in  std_logic;
        decrypt         : in  std_logic;
        bdi_ready       : out std_logic;
        bdi_valid       : in  std_logic;
        bdi_type        : in  std_logic_vector(3                -1 downto 0);
        bdi_partial     : in  std_logic;
        bdi_eot         : in  std_logic;
        bdi_eoi         : in  std_logic;
        bdi_size        : in  std_logic_vector(G_LBS_BYTES+1    -1 downto 0);
        bdi_valid_bytes : in  std_logic_vector(G_DBLK_SIZE/8    -1 downto 0);
        bdi_pad_loc     : in  std_logic_vector(G_DBLK_SIZE/8    -1 downto 0);
        --! PostProcessor
        bdo             : out std_logic_vector(G_DBLK_SIZE      -1 downto 0);
        bdo_valid       : out std_logic;
        bdo_ready       : in  std_logic;
        bdo_size        : out std_logic_vector(G_LBS_BYTES+1    -1 downto 0);
        msg_auth_done   : out std_logic;
        msg_auth_valid  : out std_logic
    );
end entity CipherCore;


architecture structure of CipherCore is
  -- Constants
  constant CONST_UNROLED_R : std_logic_vector(7 downto 0) := std_logic_vector(to_unsigned(UNROLED_ROUNDS, 8));
  constant CONST_ROUNDS_A  : std_logic_vector(7 downto 0) := std_logic_vector(to_unsigned(ROUNDS_A, 8));
  constant CONST_ROUNDS_B  : std_logic_vector(7 downto 0) := std_logic_vector(to_unsigned(ROUNDS_B, 8));
  constant CONST_ROUNDS_AmR: std_logic_vector(3 downto 0) := std_logic_vector(to_unsigned(ROUNDS_A-UNROLED_ROUNDS, 4));
  constant CONST_ROUNDS_BmR: std_logic_vector(3 downto 0) := std_logic_vector(to_unsigned(ROUNDS_B-UNROLED_ROUNDS, 4));
  constant CONST_RATE      : std_logic_vector(7 downto 0) := std_logic_vector(to_unsigned(RATE, 8));
  constant STATE_WORD_SIZE : integer := 64;
  constant KEY_SIZE        : integer := 128;
  constant CONST_KEY_SIZE  : std_logic_vector(7 downto 0) := std_logic_vector(to_unsigned(KEY_SIZE, 8));

  -- Segment Type Encoding
  constant TYPE_AD    : std_logic_vector(2 downto 0) := "000";
  constant TYPE_PTCT  : std_logic_vector(2 downto 0) := "010";
  constant TYPE_TAG   : std_logic_vector(2 downto 0) := "100";
  constant TYPE_LEN   : std_logic_vector(2 downto 0) := "101";
  constant TYPE_NONCE : std_logic_vector(2 downto 0) := "110";
    
  -- FSM state definition
  type state_t is (STATE_IDLE,
                   STATE_UPDATE_KEY,
                   STATE_WRITE_NONCE_0,
                   STATE_WRITE_NONCE_1,
                   STATE_INITIALIZATION,
                   STATE_PERMUTATION,
                   STATE_FINALIZATION,
                   STATE_WAIT_FOR_INPUT,
                   STATE_PROCESS_TAG_0,
                   STATE_PROCESS_TAG_1);
  
  -- FSM next and present state signals
  signal State_DN,State_DP      : state_t;

  -- Ascon's state registers
  signal X0_DN, X0_DP   : std_logic_vector(STATE_WORD_SIZE-1 downto 0);
  signal X1_DN, X1_DP   : std_logic_vector(STATE_WORD_SIZE-1 downto 0);
  signal X2_DN, X2_DP   : std_logic_vector(STATE_WORD_SIZE-1 downto 0);
  signal X3_DN, X3_DP   : std_logic_vector(STATE_WORD_SIZE-1 downto 0);
  signal X4_DN, X4_DP   : std_logic_vector(STATE_WORD_SIZE-1 downto 0);

  -- Key register
  signal Keyreg_DN      : std_logic_vector(KEY_SIZE-1 downto 0);
  signal Keyreg_DP      : std_logic_vector(KEY_SIZE-1 downto 0);

  -- Round counter
  signal RoundCounter_DN        : std_logic_vector(3 downto 0);
  signal RoundCounter_DP        : std_logic_vector(3 downto 0);
  signal DisableRoundCounter_S  : std_logic;

  -- Additional control logic registers
  signal IsFirstPTCT_DN, IsFirstPTCT_DP   : std_logic;
  signal IsDecryption_DN, IsDecryption_DP : std_logic;

  -- Helper function, rotates a state word
  function ROTATE_STATE_WORD (
    word            : std_logic_vector(STATE_WORD_SIZE-1 downto 0);
    constant rotate : integer)
    return std_logic_vector is
  begin  -- ROTATE_STATE_WORD
    return  word(ROTATE-1 downto 0) & word(STATE_WORD_SIZE-1 downto ROTATE);
  end ROTATE_STATE_WORD;

begin
  

  -----------------------------------------------------------------------------
  -- State operations (permutation, data loading, et cetera)
  state_opeartions_p : process (IsDecryption_DP, IsFirstPTCT_DP,
                                Keyreg_DP, RoundCounter_DP,
                                RoundCounter_DP, State_DN,
                                State_DP, X0_DP,
                                X1_DP, X2_DP, X3_DP,
                                X4_DP, bdi,
                                bdi, bdi_eot, bdi_type,
                                bdi_valid, bdi_valid_bytes, decrypt) is

    -- Roudn permutation input, intermediates, and output
    variable P0_DV, P1_DV, P2_DV, P3_DV, P4_DV : std_logic_vector(STATE_WORD_SIZE-1 downto 0);
    variable R0_DV, R1_DV, R2_DV, R3_DV, R4_DV : std_logic_vector(STATE_WORD_SIZE-1 downto 0);
    variable S0_DV, S1_DV, S2_DV, S3_DV, S4_DV : std_logic_vector(STATE_WORD_SIZE-1 downto 0);
    variable T0_DV, T1_DV, T2_DV, T3_DV, T4_DV : std_logic_vector(STATE_WORD_SIZE-1 downto 0);
    variable U0_DV, U1_DV, U2_DV, U3_DV, U4_DV : std_logic_vector(STATE_WORD_SIZE-1 downto 0);
    
    -- Round constant
    variable RoundConst_DV                     : std_logic_vector(63 downto 0);
    
    -- Second part of tag comparison
    variable TagCompResult_DV                  : std_logic_vector(RATE-1 downto 0);
    
  begin  -- process state_opeartions_p
    --- Default values:
    -- State variable X0-X4 --> keep current value
    X0_DN <= X0_DP; X1_DN <= X1_DP; X2_DN <= X2_DP; X3_DN <= X3_DP; X4_DN <= X4_DP;

    -- Permutation input --> use current state as default input
    P0_DV := X0_DP; P1_DV := X1_DP; P2_DV := X2_DP; P3_DV := X3_DP; P4_DV := X4_DP;
 
    -- Reset informational signals, when perfoming initialization
    if State_DP = STATE_INITIALIZATION then
      IsFirstPTCT_DN  <= '1';
      IsDecryption_DN <= decrypt;
    else -- otherwise just keep value
      IsFirstPTCT_DN  <= IsFirstPTCT_DP;
      IsDecryption_DN <= IsDecryption_DP;
    end if;
    
    --- P0,[P1] MUX: When data input is ready --> select P0,[P1] accordingly
    if State_DP = STATE_WAIT_FOR_INPUT and (bdi_valid = '1') then
      -- Encryption
      if (IsDecryption_DP = '0') or (bdi_type = TYPE_AD) then
        if RATE = 128 then -- Ascon128a variant
          P0_DV := X0_DP xor bdi(RATE-1 downto RATE-64);
          P1_DV := X1_DP xor bdi(63 downto 0);
        else
          P0_DV := X0_DP xor bdi;
        end if;
      -- Decryption
      else
        -- We need to take care of the number of valid bytes!
        for i in 7 downto 0 loop
          if RATE = 128 then            -- Ascon128a variant
            -- P1 xor input data ...
            P0_DV(i*8 + 7 downto i*8) := bdi(i*8 + 7 + 64 downto i*8 + 64);
            P1_DV(i*8 + 7 downto i*8) := bdi(i*8 + 7      downto i*8);
            -- if invalid byte then use additionally xor X1
            if bdi_valid_bytes(i) = '0' then
              P1_DV(i*8 + 7 downto i*8) := P1_DV(i*8 + 7 downto i*8) xor X1_DP(i*8 + 7 downto i*8);
            end if;
            -- if invalid byte then use additionally xor X0
            if bdi_valid_bytes(i+8) = '0' then
              P0_DV(i*8 + 7 downto i*8) := P0_DV(i*8 + 7 downto i*8) xor X0_DP(i*8 + 7 downto i*8);
            end if;
          else                          -- Ascon128 variant
            -- P0 xor input data ...
            P0_DV(i*8 + 7 downto i*8) := bdi(i*8 + 7 downto i*8);
            -- if invalid byte then use additionally xor X0
            if bdi_valid_bytes(i) = '0' then
              P0_DV(i*8 + 7 downto i*8) := P0_DV(i*8 + 7 downto i*8) xor X0_DP(i*8 + 7 downto i*8);
            end if;
          end if;
        end loop;  -- i
      end if;

      --- P1[2]-4 MUX:
      -- if performing FINALIZATION next
      if State_DN = STATE_FINALIZATION then
        -- TODO: when 128a variant write P2
        -- Add key before permutation
        if RATE = 64 then               -- Ascon128 variant
          P1_DV := P1_DV xor Keyreg_DP(127 downto 64);
          P2_DV := P2_DV xor Keyreg_DP(63 downto 0);
        else                            -- Ascon128a variant
          P2_DV := P2_DV xor Keyreg_DP(127 downto 64);
          P3_DV := P3_DV xor Keyreg_DP(63 downto 0);
        end if;
        
        -- If first PT/CT never processed (empty AD + PT/CT case)
        if (IsFirstPTCT_DP = '1') then
          P4_DV(0) := not P4_DV(0);     -- Add 0*||1
        end if;
      -- if performing PERMUTATION next
      elsif ((bdi_valid = '1') and not ((bdi_type = TYPE_TAG) or
             (bdi_type = TYPE_PTCT and bdi_eot = '1'))) then
        -- ... and first round of PT/CT calculation
        if (bdi_type = TYPE_PTCT) and (IsFirstPTCT_DP = '1') then
          IsFirstPTCT_DN <= '0';        -- SET first PT/CT performed
          P4_DV(0) := not P4_DV(0);     -- Add 0*||1
        end if;   
      end if;
    end if;

    -- Unroled round permutation
    for r in 0 to UNROLED_ROUNDS-1 loop
      -- Calculate round constant
      RoundConst_DV             := (others => '0');  -- set to zero
      RoundConst_DV(7 downto 0) := not std_logic_vector(unsigned(RoundCounter_DP(3 downto 0)) + r) &
                                       std_logic_vector(unsigned(RoundCounter_DP(3 downto 0)) + r);
    
      R0_DV := P0_DV xor P4_DV;
      R1_DV := P1_DV;
      R2_DV := P2_DV xor P1_DV xor RoundConst_DV;
      R3_DV := P3_DV;
      R4_DV := P4_DV xor P3_DV;

      S0_DV := R0_DV xor (not R1_DV and R2_DV);
      S1_DV := R1_DV xor (not R2_DV and R3_DV);
      S2_DV := R2_DV xor (not R3_DV and R4_DV);
      S3_DV := R3_DV xor (not R4_DV and R0_DV);
      S4_DV := R4_DV xor (not R0_DV and R1_DV);

      T0_DV := S0_DV xor S4_DV;
      T1_DV := S1_DV xor S0_DV;
      T2_DV := not S2_DV;
      T3_DV := S3_DV xor S2_DV;
      T4_DV := S4_DV;

      U0_DV := T0_DV xor ROTATE_STATE_WORD(T0_DV, 19) xor ROTATE_STATE_WORD(T0_DV, 28);
      U1_DV := T1_DV xor ROTATE_STATE_WORD(T1_DV, 61) xor ROTATE_STATE_WORD(T1_DV, 39);
      U2_DV := T2_DV xor ROTATE_STATE_WORD(T2_DV, 1)  xor ROTATE_STATE_WORD(T2_DV, 6);
      U3_DV := T3_DV xor ROTATE_STATE_WORD(T3_DV, 10) xor ROTATE_STATE_WORD(T3_DV, 17);
      U4_DV := T4_DV xor ROTATE_STATE_WORD(T4_DV, 7)  xor ROTATE_STATE_WORD(T4_DV, 41);

      P0_DV := U0_DV;
      P1_DV := U1_DV;
      P2_DV := U2_DV;
      P3_DV := U3_DV;
      P4_DV := U4_DV;
    end loop;

   -- Do tag comparison when doing decryption in PROCESS TAG states
   msg_auth_done  <= '0'; --default
   msg_auth_valid <= '0';
   if IsDecryption_DP = '1' then
     if (State_DP = STATE_PROCESS_TAG_0) then
       -- valid data for comparison ready?
       if bdi_valid = '1' then
         if RATE = 128 then             -- Ascon128a variant
           -- signal we are done with comparison
           msg_auth_done <= '1';
           -- tags equal?
           TagCompResult_DV := (X3_DP & X4_DP) xor Keyreg_DP;
           if (TagCompResult_DV = bdi) then
             msg_auth_valid <= '1';
           end if;
         else                           -- Ascon128 variant
           X3_DN <= X3_DP xor Keyreg_DP(127 downto 64) xor bdi;
         end if;
       end if;
     elsif (State_DP = STATE_PROCESS_TAG_1) then
       -- debug
       if RATE = 64 then               -- Ascon128 variant
         -- valid data for comparison ready?
         if bdi_valid = '1' then
           -- signal we are done with comparison
           msg_auth_done    <= '1';
           -- Check if tags are equal
           TagCompResult_DV := (X4_DP xor Keyreg_DP(63 downto 0) xor bdi);
           if (X3_DP & TagCompResult_DV) = x"00000000000000000000000000000000" then
             msg_auth_valid <= '1';
           end if;
         end if;
       end if;
     end if;
   end if;


   --- State X0...4 MUX: select input of state registers
   case State_DP is
     -- WRITE_NONCE_0 --> and init state 
     when STATE_WRITE_NONCE_0 =>
       -- ready to receive
       if (bdi_valid = '1') then
         -- fill X0 with IV
         X0_DN <= CONST_KEY_SIZE & CONST_RATE & CONST_ROUNDS_A & CONST_ROUNDS_B & x"00000000";
         X1_DN <= Keyreg_DP(127 downto 64);
         X2_DN <= Keyreg_DP(63 downto 0);
         
         if RATE = 128 then -- Ascon128a variant
           X3_DN <= bdi(RATE-1 downto RATE-64);
           X4_DN <= bdi( 63 downto 0);
         else               -- Ascon128 variant
           X3_DN <= bdi(63 downto 0);
         end if;
       end if;

     -- WRITE_NONCE_1 --> second part of nonce
     when STATE_WRITE_NONCE_1 =>
       -- ready to receive
       if (bdi_valid = '1') then
         X4_DN <= bdi;
       end if;

     -- INITIALIZATION, PERMUTATION, FINALIZATION --> apply round transformation
     when STATE_PERMUTATION | STATE_INITIALIZATION | STATE_FINALIZATION =>
       X0_DN <= P0_DV; X1_DN <= P1_DV; X2_DN <= P2_DV;
       -- Add key after initialization
       if (State_DP = STATE_INITIALIZATION and RoundCounter_DP = CONST_ROUNDS_AmR) then
         X3_DN <= P3_DV xor Keyreg_DP(127 downto 64);
         X4_DN <= P4_DV xor Keyreg_DP(63 downto 0);
       else
         X3_DN <= P3_DV;
         X4_DN <= P4_DV;
       end if;
       
     -- WAIT FOR INPUT --> apply round transformation when input is ready
     when STATE_WAIT_FOR_INPUT =>
       if bdi_valid = '1' then
         -- State <= permutation output
         X0_DN <= P0_DV; X1_DN <= P1_DV; X2_DN <= P2_DV; X3_DN <= P3_DV; X4_DN <= P4_DV;
       end if;
     when others => null;
   end case;

  end process state_opeartions_p;

  -----------------------------------------------------------------------------
  -- Update key register --> simple shift register
  key_update_p: process (Keyreg_DP, State_DP, key, key_valid) is
  begin  -- process key_update_p
    Keyreg_DN <= Keyreg_DP; -- default
    key_ready <= '0';
    
    -- only update key while in the update state
    if State_DP = STATE_UPDATE_KEY then
      key_ready <= '1'; -- always ready
      -- shift register and insert new key data
      if key_valid = '1' then
        Keyreg_DN <= Keyreg_DP (KEY_SIZE-G_KEY_SIZE-1 downto 0) & key;
      end if;
    end if;
  end process key_update_p;

  -----------------------------------------------------------------------------
  -- Input logic --> controlling input interface signals
  input_logic_p: process (State_DP, bdi_type, bdi_valid) is
  begin  -- process input_logic_p
    bdi_ready <= '1'; -- default, ready

    -- We are busy when...
    if State_DP = STATE_IDLE or
       State_DP = STATE_UPDATE_KEY or
       State_DP = STATE_INITIALIZATION or
       State_DP = STATE_PERMUTATION or
      (State_DP = STATE_PROCESS_TAG_0 and IsDecryption_DP = '0') or
      (State_DP = STATE_PROCESS_TAG_1 and IsDecryption_DP = '0') or
       State_DP = STATE_FINALIZATION then
      -- signal busyness
      bdi_ready <= '0'; 
    end if;
  end process input_logic_p;

  -----------------------------------------------------------------------------
  -- Output logic --> controlling output interface signals
  output_logic_p: process (Keyreg_DP,
                           State_DP, X0_DP, X1_DP, X3_DP, X4_DP, bdi,
                           bdi, bdi_size,
                           bdi_type, bdi_valid, bdo_ready) is
  begin  -- process output_logic_p
    bdo_valid <= '0'; -- default, nothing to output
    if RATE = 128 then                  -- Ascon128a variant
      bdo(RATE-1 downto RATE-64) <= X0_DP xor bdi(RATE-1 downto RATE-64);
      bdo(63 downto 0)           <= X1_DP xor bdi(63 downto 0);
    else                                -- Ascon128 variant
      bdo <= X0_DP xor bdi;
    end if;

    -- Wait for output to be ready
    if bdo_ready = '1' then
      -- waiting for output of PT/CT and there is something to output (size > 0)?
      if (State_DP = STATE_WAIT_FOR_INPUT) then
        if (bdi_valid = '1') and (bdi_type = TYPE_PTCT) and (bdi_size /= (bdi_size'range => '0')) then
          bdo_valid <= '1';
        end if;
      -- output Tag part 1
      elsif State_DP = STATE_PROCESS_TAG_0 then
        bdo_valid <= '1';
        if RATE = 128 then              -- Ascon128a variant
          bdo(RATE-1 downto RATE-64) <= X3_DP xor Keyreg_DP(127 downto 64);
          bdo(63 downto 0)           <= X4_DP xor Keyreg_DP(63 downto 0);
        else                            -- Ascon128 variant
          bdo <= X3_DP xor Keyreg_DP(127 downto 64);
        end if;
      -- output Tag part 2
      elsif State_DP = STATE_PROCESS_TAG_1 then
        bdo_valid <= '1';
        bdo       <= X4_DP xor Keyreg_DP(63 downto 0);
      end if;
    end if;
  end process output_logic_p;

  -----------------------------------------------------------------------------
  -- Next state logic
  fsm_comb_p: process (IsDecryption_DP, RoundCounter_DP, State_DP, bdi_eot,
                       bdi_type, bdi_valid, bdo_ready, key_update, key_valid) is
  begin  -- process fsm_comb_p
    State_DN <= State_DP; -- default

    -- FSM state transfers
    case State_DP is
      -- IDLE
      when STATE_IDLE =>
        -- preprocessor forces key update
        if (key_update = '1') then
          State_DN <= STATE_UPDATE_KEY;
        -- skip key update and start writing the nonce
        elsif (bdi_valid = '1') and (bdi_type = TYPE_NONCE) then
          State_DN <= STATE_WRITE_NONCE_0;
        end if;

      -- UPDATE_KEY  
      when STATE_UPDATE_KEY =>
        -- when key is invalid we assume the key update is finished
        if (key_valid = '0' and key_update = '0') then
          -- go back to IDLE
          State_DN <= STATE_IDLE;
        end if;

      -- WRITE_NONCE_0 (Part 1)
      when STATE_WRITE_NONCE_0 =>
        -- write first part of nonce
        if (bdi_valid = '1')  then
          if RATE = 128 then -- Ascon128a variant
            State_DN <= STATE_INITIALIZATION;
          else
            State_DN <= STATE_WRITE_NONCE_1;
          end if;
        end if;
        
      -- WRITE_NONCE_1 (Part 2)
      when STATE_WRITE_NONCE_1 =>
        -- write second part of nonce
        if (bdi_valid = '1')  then
          State_DN <= STATE_INITIALIZATION;
        end if;

      -- INITIALIZATION
      when STATE_INITIALIZATION =>
        if RoundCounter_DP = CONST_ROUNDS_AmR then
          State_DN <= STATE_WAIT_FOR_INPUT;
        end if;
        
      -- PERMUTATION
      when STATE_PERMUTATION =>
        if RoundCounter_DP = CONST_ROUNDS_BmR then
          State_DN <= STATE_WAIT_FOR_INPUT;
        end if;

      -- FINALIZATION
      when STATE_FINALIZATION =>
        if RoundCounter_DP = CONST_ROUNDS_AmR then
          State_DN <= STATE_PROCESS_TAG_0;
        end if;

      -- PROCESS TAG PART 1
      when STATE_PROCESS_TAG_0 =>
        -- encryption -> wait for output stream to be ready
        if (IsDecryption_DP = '0') then
          if (bdo_ready = '1') then
            if RATE = 128 then          -- Ascon128a variant
              State_DN <= STATE_IDLE;
            else                        -- Ascon128 variant
              State_DN <= STATE_PROCESS_TAG_1;
            end if;
          end if;
        else
          -- decryption -> wait for tag for comparison
          if (bdi_valid = '1') then
            if RATE = 128 then          -- Ascon128a variant
              State_DN <= STATE_IDLE;
            else                        -- Ascon128 variant
              State_DN <= STATE_PROCESS_TAG_1;
            end if;
          end if;
        end if;

      -- PROCESS TAG PART 2
      when STATE_PROCESS_TAG_1 =>
        -- encryption -> wait for output stream to be ready
        if (IsDecryption_DP = '0') then
          if (bdo_ready = '1') then
            State_DN <= STATE_IDLE;
          end if;
        else
          -- decryption -> wait for tag for comparison
          if (bdi_valid = '1') then
            State_DN <= STATE_IDLE;
          end if;
        end if;
        
      -- WAIT_FOR_INPUT
      when STATE_WAIT_FOR_INPUT =>
        -- input is ready so process
        if (bdi_valid = '1') then
          -- if its a tag or last PT/CT then...
          if (bdi_type = TYPE_TAG) or
             (bdi_type = TYPE_PTCT and bdi_eot = '1')then
            State_DN <= STATE_FINALIZATION;
          else -- process AD or PT/CT
            -- PERMUTATION state is unnecessary if we fully unroll
            if (UNROLED_ROUNDS /= ROUNDS_B) then
              State_DN <= STATE_PERMUTATION;
            end if;
          end if;
        end if;
      when others => null;
    end case;
  end process fsm_comb_p;

  -----------------------------------------------------------------------------
  -- Round counter realization
  round_counter_p: process (DisableRoundCounter_S, RoundCounter_DP, State_DP,
                            bdi_valid) is
    variable CounterVal_DV : integer;
  begin  -- process round_counter_p
    RoundCounter_DN <= RoundCounter_DP; -- default

    -- Enable counter during ...
    if (State_DP = STATE_PERMUTATION) or
      (State_DP = STATE_INITIALIZATION) or
      (State_DP = STATE_FINALIZATION) or
      (State_DP = STATE_WAIT_FOR_INPUT and bdi_valid = '1') then

      -- Counter  += #unroled rounds
      CounterVal_DV := to_integer(unsigned(RoundCounter_DP)) + UNROLED_ROUNDS;  -- increment

      -- Overrun detection --> set back to 0
      if (CounterVal_DV >= ROUNDS_A) or (State_DP = STATE_PERMUTATION and CounterVal_DV >= ROUNDS_B) then
        CounterVal_DV := 0;
      end if;

      -- set next counter register value  
      RoundCounter_DN <= std_logic_vector(to_unsigned(CounterVal_DV, RoundCounter_DN'length));
      
    else -- Disable round counter and set it to zero
      RoundCounter_DN <= (others => '0');
    end if;
  end process round_counter_p;

  -----------------------------------------------------------------------------
  -- Process for all registers in design
  gen_register_with_asynchronous_reset : if G_ASYNC_RSTN = false generate
    register_process_p : process (clk, rst) is
    begin  -- process register_process_p
      if rst = '1' then                   -- asynchronous reset (active high)
        State_DP        <= STATE_IDLE;
        X0_DP           <= (others => '0');
        X1_DP           <= (others => '0');
        X2_DP           <= (others => '0');
        X3_DP           <= (others => '0');
        X4_DP           <= (others => '0');
        Keyreg_DP       <= (others => '0');
        RoundCounter_DP <= (others => '0');
        IsFirstPTCT_DP  <= '1';
        IsDecryption_DP <= '0';
      elsif clk'event and clk = '1' then  -- rising clock edge
        State_DP        <= State_DN;
        X0_DP           <= X0_DN;
        X1_DP           <= X1_DN;
        X2_DP           <= X2_DN;
        X3_DP           <= X3_DN;
        X4_DP           <= X4_DN;
        Keyreg_DP       <= Keyreg_DN;
        RoundCounter_DP <= RoundCounter_DN;
        IsFirstPTCT_DP  <= IsFirstPTCT_DN;
        IsDecryption_DP <= IsDecryption_DN;
      end if;
    end process register_process_p;
  end generate gen_register_with_asynchronous_reset;
   -- else generate with synchronous reset
  gen_register_with_synchronous_reset : if G_ASYNC_RSTN = true generate
    register_process_p : process (clk, rst) is
    begin  -- process register_process_p
      if clk'event and clk = '1' then   -- rising clock edge
        if rst = '1' then               -- synchronous reset (active high)
          State_DP        <= STATE_IDLE;
          X0_DP           <= (others => '0');
          X1_DP           <= (others => '0');
          X2_DP           <= (others => '0');
          X3_DP           <= (others => '0');
          X4_DP           <= (others => '0');
          Keyreg_DP       <= (others => '0');
          RoundCounter_DP <= (others => '0');
          IsFirstPTCT_DP  <= '1';
          IsDecryption_DP <= '0';
        else
          State_DP        <= State_DN;
          X0_DP           <= X0_DN;
          X1_DP           <= X1_DN;
          X2_DP           <= X2_DN;
          X3_DP           <= X3_DN;
          X4_DP           <= X4_DN;
          Keyreg_DP       <= Keyreg_DN;
          RoundCounter_DP <= RoundCounter_DN;
          IsFirstPTCT_DP  <= IsFirstPTCT_DN;
          IsDecryption_DP <= IsDecryption_DN;
        end if;
      end if;
    end process register_process_p;
  end generate gen_register_with_synchronous_reset;
end structure;
