Change following parameters for AEAD

Ascon-128
G_ABLK_SIZE    : integer := 64;
G_DBLK_SIZE    : integer := 64;

Ascon-128a
G_ABLK_SIZE    : integer := 128;
G_DBLK_SIZE    : integer := 128;

------------------------------------------
Change following parameters for CipherCore

Ascon-128
RATE           : integer := 64;
UNROLED_ROUNDS : integer := [1,2,3,6]; -- select 
ROUNDS_A       : integer := 12;
ROUNDS_B       : integer := 6;

Ascon-128a
RATE           : integer := 128;
UNROLED_ROUNDS : integer := [1,2,4]; -- select 
ROUNDS_A       : integer := 12;
ROUNDS_B       : integer := 8;
