---------------------------------------------------------------------------------
-- DE10_lite Tiny USB Full Speed decoder by Dar (darfpga@net-c.fr) (05/12/2021)
-- http://darfpga.blogspot.fr
---------------------------------------------------------------------------------
-- Educational use only
-- Use at your own risk. Beware voltage translation or protection are required
---------------------------------------------------------------------------------
---------------------------------------------------------------------------------
-- Main features : Tiny USB decoder for Full Speed USB devices (12Mbit/s)
--
--  USB captured frames are displayed on nios2-terminal thru Jtag interface.
--  Use Jtag-uart to display data and send user commands to decoder/filter.
--  Data buffer 8ko within Jtag-uart FIFO.
--  Seems to be no data loss with small devices (keyboard, joysitck, mouse)
--  Allow to capture Setup frames for analysis.
--
--  Reset decoder  : key(0)
--  Reinit USB bus : key(1) will restart device enumeration
--
--  Display
--    HEX 1-0 : Last key (cmd) entered in nios2-terminal
--    HEX 3-2 : USB SOF frame counter (7 MSB only)
--    HEX   4 : Max capture lines
--	   HEX   5 :
--       segment 0 : token packet filter on/off
--       segment 1 : sof   packet filter on/off
--       segment 2 : data  packet filter on/off
--       segment 4 : setup packet filter on/off
--
--  Commands (via nios2-terminal)
--		key '1' : toggle token packet filter 
--		key '2' : toggle sof   packet filter 
--		key '3' : toggle data  packet filter 
--		key '4' : toggle setup packet filter 
--
--    key 'space' : toggle all active filters on/off
--
--		key '6' : trigger/restart acquistion after stop (single shot)
--		key '7' : +32 lines to max capture buffer (wrap to 0 after 15) 
--              (0 = continous)
--		key '8' : -32 lines to max capture buffer (wrap to 15 after 0)
--              (0 = continous)
---------------------------------------------------------------------------------
-- Hardware wiring
---------------------------------------------------------------------------------
--  Operating as a spy tool USB power supply *must NOT* be connected to DE10 board.
--  Only D+ and D- have to be connected to the DE10 board gpio.
--
--  If the USB port to be spyied is connected on the same computer as the display
--  computer (nios2terminal via Jtag-uart on USB BLASTER port there is no need
--  to connect the USB ground wire to the DE10 board GND.
--
--  In other cases make sure that there *NO CURRENT FLOW* between the display 
--  machine and the USB to be spyied before connected the DE10 ground to the USB
--  ground. You might have to use isolation transformers for human and hardware
--  safety.
--
--  On DE10_LITE (only)
--
-- 	 D+ - green wire to gpio(0) pin #1 thru voltage translation/protection
-- 	 D- - white wire to gpio(2) pin #3 thru voltage translation/protection
--
---------------------------------------------------------------------------------
--
-- Voltage protection with Schottky diodes     BAT54S  (A2) o--|>{--o--|>{--O (K1)
--                                                                  |
--  use 2 x BAT54S or 4 x BAT42                                  (K2-A1)
--    + 2 x 47 Ohms
---------------------------------------------------------------------------------
--                              --------
--   gpio(0) pin #1  o-------o--| 47 Ohms|---o D+ USB to spy (green)
--                           |   --------
--                           |
--       gnd pin #30 o--|>{--o  BAT54S
--                           |
--     +3.3V pin #29 o--}<|---
--
--                              --------
--   gpio(2) pin #3  o-------o--| 47 Ohms|---o D- USB to spy (white)
--                           |   --------
--                           |
--       gnd pin #30 o--|>{--o
--                           |
--     +3.3V pin #29 o--}<|---
--
---------------------------------------------------------------------------------
-- Jtag-uart from QSys
---------------------------------------------------------------------------------
--	Jtag-uart comes from Qsys. It can be rebuilt with Qsys from scracth :
--
--		- Launch Qsys
--		- Remove Clock source component
--		- Add Jtag uart component from IP_catalog Interface_Protocols\serial
--    - Choose Wite FIFO buffer depth
--    - Double-click on each 4 lines of column 'Export' (lines : Clk, reset, 
--      avalon_jtag_slave, irq)
--    - Click on Generate HDL
--    - Select HDL design files for synthesis => VHDL
--    - Uncheck Create block symbol file (.bsf)
--		- Set Ouput_directory
--		- Click on Generate, Give name jtag_uart_8kw.qsys
--		- Wait generation completed and close box when done
--    - Click on Finish in Qsys main windows
--
--    - Insert qsys/jtag_uart_8kw/synthesis/jtag_uart_8kw.qip Quartus project
--
--    - Modify jtag_uart_8kw.vhd in Quartus to simplify names for entity 
--      and component declaration
--        first replace any jtag_uart_0_avalon_jtag_ with av_
--        then remove any remaining jtag_uart_0_
--
---------------------------------------------------------------------------------
-- Known bugs
---------------------------------------------------------------------------------
--  Carriage return / line feed missing after some packets due to early ot late 
--  end-of-packet Se0. 
---------------------------------------------------------------------------------
---------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;
use ieee.numeric_std.all;

library work;

entity jtag_uart_de10_lite is
port(
 max10_clk1_50  : in std_logic;
-- max10_clk2_50  : in std_logic;
-- adc_clk_10     : in std_logic;
 ledr           : out std_logic_vector(9 downto 0);
 key            : in std_logic_vector(1 downto 0);
 sw             : in std_logic_vector(9 downto 0);

-- dram_ba    : out std_logic_vector(1 downto 0);
-- dram_ldqm  : out std_logic;
-- dram_udqm  : out std_logic;
-- dram_ras_n : out std_logic;
-- dram_cas_n : out std_logic;
-- dram_cke   : out std_logic;
-- dram_clk   : out std_logic;
-- dram_we_n  : out std_logic;
-- dram_cs_n  : out std_logic;
-- dram_dq    : inout std_logic_vector(15 downto 0);
-- dram_addr  : out std_logic_vector(12 downto 0);

 hex0 : out std_logic_vector(7 downto 0);
 hex1 : out std_logic_vector(7 downto 0);
 hex2 : out std_logic_vector(7 downto 0);
 hex3 : out std_logic_vector(7 downto 0);
 hex4 : out std_logic_vector(7 downto 0);
 hex5 : out std_logic_vector(7 downto 0);

-- vga_r     : out std_logic_vector(3 downto 0);
-- vga_g     : out std_logic_vector(3 downto 0);
-- vga_b     : out std_logic_vector(3 downto 0);
-- vga_hs    : out std_logic;
-- vga_vs    : out std_logic;
 
-- gsensor_cs_n : out   std_logic;
-- gsensor_int  : in    std_logic_vector(2 downto 0); 
-- gsensor_sdi  : inout std_logic;
-- gsensor_sdo  : inout std_logic;
-- gsensor_sclk : out   std_logic;

-- arduino_io      : inout std_logic_vector(15 downto 0); 
-- arduino_reset_n : inout std_logic;
 
 gpio          : inout std_logic_vector(35 downto 0)
);
end jtag_uart_de10_lite;

architecture struct of jtag_uart_de10_lite is

-- Jtag UART from QSYS (8k fifo from avalon side to JTAG interface)
component jtag_uart_8kw is
port (
	av_chipselect  : in  std_logic                     := 'X';             -- chipselect
	av_address     : in  std_logic                     := 'X';             -- address
	av_read_n      : in  std_logic                     := 'X';             -- read_n
	av_readdata    : out std_logic_vector(31 downto 0);                    -- readdata
	av_write_n     : in  std_logic                     := 'X';             -- write_n
	av_writedata   : in  std_logic_vector(31 downto 0) := (others => 'X'); -- writedata
	av_waitrequest : out std_logic;                                        -- waitrequest
	clk_clk        : in  std_logic                     := 'X';             -- clk
	irq_irq        : out std_logic;                                        -- irq
	reset_reset_n  : in  std_logic                     := 'X'              -- reset_n
);
end component jtag_uart_8kw;

signal clock_usb  : std_logic;
signal clock_30  : std_logic;
signal reset     : std_logic;
 
alias reset_n    : std_logic is key(0);
alias dp         : std_logic is gpio(0);
alias dm         : std_logic is gpio(2);

signal uart_chipselect  : std_logic := '0';             -- chipselect
signal uart_address     : std_logic := '0';             -- address
signal uart_read_n      : std_logic := '1';             -- read_n
signal uart_readdata    : std_logic_vector(31 downto 0);-- readdata
signal uart_write_n     : std_logic := '1';             -- write_n
signal uart_writedata   : std_logic_vector(31 downto 0) := (others => '0'); -- writedata
signal uart_waitrequest : std_logic;                  -- waitrequest
signal uart_irq         : std_logic;                  -- irq
signal uart_stm : std_logic_vector(3 downto 0);

signal read_data : std_logic_vector(7 downto 0);
signal uart_write_request : std_logic;

signal dp_r      : std_logic;
signal dp_rr     : std_logic;
signal se0       : std_logic;
signal se0_r     : std_logic;
signal se0_rr    : std_logic;
signal usb_eop   : std_logic;
signal usb_sleep     : std_logic;
signal usb_cnt       : std_logic_vector(7 downto 0);
signal usb_bit_stuff : std_logic;

signal usb_bit_cnt   : std_logic_vector(7 downto 0);
signal usb_bit_r     : std_logic;
signal usb_shift_reg : std_logic_vector(15 downto 0); 

signal usb_pid     : std_logic_vector(3 downto 0); 
signal usb_adr     : std_logic_vector(6 downto 0); 
signal usb_ep      : std_logic_vector(3 downto 0); 
signal usb_frame   : std_logic_vector(10 downto 0);
signal usb_data    : std_logic_vector(7 downto 0); 

signal usb_crc5    : std_logic_vector(4 downto 0);
signal usb_crc16   : std_logic_vector(15 downto 0);
signal usb_crc_ok  : std_logic;

signal usb_sof_cnt   : std_logic_vector(15 downto 0);
signal usb_msg_cnt   : std_logic_vector(15 downto 0);
signal usb_setup_cnt : std_logic_vector(15 downto 0);

signal new_data_cnt  : std_logic_vector(7 downto 0);
signal write_seq     : std_logic_vector(1  downto 0);
signal data_for_uart : std_logic_vector(17 downto 0);
signal uart_byte     : std_logic_vector(7 downto 0);

signal get_data     : std_logic;
signal want_nothing : std_logic := '0';
signal want_token   : std_logic := '1';
signal want_sof     : std_logic := '1';
signal want_data    : std_logic := '1';
signal want_setup   : std_logic := '1';
signal line_cnt     : std_logic_vector(9 downto 0);
signal max_line     : std_logic_vector(3 downto 0);

begin

--arduino_io not used pins
--arduino_io(7) <= '1'; -- to usb host shield max3421e RESET
--arduino_io(8) <= 'Z'; -- from usb host shield max3421e GPX
--arduino_io(9) <= 'Z'; -- from usb host shield max3421e INT
--arduino_io(13) <= 'Z'; -- not used
--arduino_io(14) <= 'Z'; -- not used

-- Clock 30MHz
clocks : entity work.max10_pll_30M_3p58M
port map(
 inclk0 => max10_clk1_50,
 c0 => clock_30, -- 30MHz
 c1 => open,     -- 3p58MHz
 locked => open  -- pll_locked
);

clocks_usb : entity work.max10_pll_96M_48M_24M
port map(
 inclk0 => max10_clk1_50,
 c0 => clock_usb,  -- 96MHz = 8*12MHz. Period ~10.42ns (8 times oversample signal)
 c1 => open,       -- 48MHz
 c2 => open,       -- 24MHz
 locked => open    -- pll_locked
);

reset <= not reset_n;
						  						  
-- low signal level usb bus activity
dp <= 'Z' when key(1) = '1' else '0'; -- key(1) => reset usb bus
dm <= 'Z' when key(1) = '1' else '0';

se0 <= not(dp or dm);

process (clock_usb)
begin
	if rising_edge(clock_usb) then

		se0_r  <= se0;
		se0_rr <= se0_r;
		
		-- End of packet detection.
		if (se0_r = '1') and (se0_rr = '1') then
			usb_eop <= '1';
		else
			usb_eop <= '0';
		end if;
		
		dp_r <= dp;
		dp_rr <= dp_r;

		-- Usb sleep after end of packet and after ~830ns inactivity
		-- wakeup as soon as activity (always start with dp = '0')
		if usb_eop = '1' then 
			usb_sleep <= '1';
		elsif dp_r = '0' then
			usb_sleep <= '0';
		elsif usb_cnt > 80 then 
			usb_sleep <= '1';
		end if;
		
		-- Reset usb_cnt when sleep and after each signal (dp) change.
		-- If change occures after 6 bits (48 clk inactivity ~500ns)
		-- then this is a 'bit stuff' change => data bit to be ignored.
		if (usb_sleep = '1') or (dp_r /= dp_rr ) then 
			usb_cnt <= (others => '0');
			if usb_cnt > 48 then
				usb_bit_stuff <= '1';
			end if;
		else
			usb_cnt <= usb_cnt + 1;
		end if;

		-- Keep bit stuff until signal analysis but not more
		if usb_cnt = 6 then
			usb_bit_stuff <= '0';
		end if;

		-- Note : usb_cnt is reset to a 0 after each signal change.
		-- usb_cnt cannot derive widely from actual bus speed since there is
		--	at least one signal change every 6 data bits
		
	end if;
end process;

-- bit level usb bus activity
-- manage shift register data and CRCs
process (clock_usb)
begin
	if rising_edge(clock_usb) then
--		if usb_sleep = '1' then
		if (usb_sleep = '1') and (usb_eop = '0') then
			usb_bit_cnt <= (others => '0');
			usb_bit_r <= '1';
			usb_shift_reg <= (others => '0');
		else
			-- latch current signal around mid bit period (1 bit period = 8 clk)
			if (usb_cnt(2 downto 0) = 3)  then 
				usb_bit_r <= dp_r;
			end if;
			
			-- at mid bit period analyse signal change only when there is 
			-- no bit stuffing
			if (usb_cnt(2 downto 0) = 3) and (usb_bit_stuff = '0') then 
				-- increment bit counter (ignore stuffing bit)
				-- no need to count past 255 but should avoid wrap back
				if usb_bit_cnt < 255 then 
					usb_bit_cnt <= usb_bit_cnt + 1;
				end if;
				
				-- if there is *NO* signal change => shift data in with '1'
				-- always left shift CRC
				-- and XOR CRC with polynome if MSB of current CRC is '0'
				if usb_bit_r = dp_r then
					usb_shift_reg <= '1' & usb_shift_reg(15 downto 1);
					
					if usb_crc5(4) = '0' then
						usb_crc5 <= usb_crc5(3 downto 0) &'0' xor "00101";
					else
						usb_crc5 <= usb_crc5(3 downto 0) &'0';
					end if;
					
					if usb_crc16(15) = '0' then
						usb_crc16 <= usb_crc16(14 downto 0) &'0' xor "1000000000000101";
					else
						usb_crc16 <= usb_crc16(14 downto 0) &'0';
					end if;
					
				-- if there is a signal change => shift data in with '0'
				-- always left shift CRC
				-- and XOR CRC with polynome if MSB of current CRC is '1'					
				else
					usb_shift_reg <= '0' & usb_shift_reg(15 downto 1);
					
					if usb_crc5(4) = '1' then
						usb_crc5 <= usb_crc5(3 downto 0) &'0' xor "00101";
					else
						usb_crc5 <= usb_crc5(3 downto 0) &'0';
					end if;
					
					if usb_crc16(15) = '1' then
						usb_crc16 <= usb_crc16(14 downto 0) &'0' xor "1000000000000101";
					else
						usb_crc16 <= usb_crc16(14 downto 0) &'0';
					end if;
										
				end if;
				
				-- Initialise CRC with all '1' at beginning of packet.
				-- CRCs computation will start with 16th bit.
				if usb_bit_cnt < 16 then
					usb_crc5 <= (others => '1');
					usb_crc16 <= (others => '1');
				end if;
			end if;
		end if;
	end if;
end process;

-- latch useful data from shift register at the right moment
process (clock_usb)
begin

	if reset = '1' then
		usb_pid       <= (others => '0');
		usb_adr       <= (others => '0');
		usb_ep        <= (others => '0');
		usb_frame     <= (others => '0');
		usb_data      <= (others => '0');
		usb_crc_ok    <= '0';
		usb_sof_cnt   <= (others => '0');
		usb_msg_cnt   <= (others => '0');
		usb_setup_cnt <= (others => '0');
		new_data_cnt  <= (others => '0'); -- fast counter reseted at each new byte
	
	elsif rising_edge(clock_usb) then
	
		-- No need to count past 31
		if (new_data_cnt < 31) then new_data_cnt <= new_data_cnt + 1; end if;

		-- Latch pid
		if usb_bit_cnt = X"10" then
			usb_pid <= usb_shift_reg(11 downto 8);
		end if;
		
		-- Latch new data (byte) and reset fast counter
		if (usb_bit_cnt(2 downto 0) = "000") and (usb_sleep = '0') then
			usb_data <= usb_shift_reg(15 downto 8);
			if (new_data_cnt = 31) then new_data_cnt <= (others => '0'); end if;
		end if;
		
		-- Latch device address and end point on PING/OUT/IN/SETUP token
		if (usb_pid = x"4") or (usb_pid = x"1") or (usb_pid = x"9") or (usb_pid = x"D") then
			if usb_bit_cnt = X"17" then
				usb_adr <= usb_shift_reg(15 downto 9);
			end if;

			if usb_bit_cnt = X"1B" then
				usb_ep <= usb_shift_reg(15 downto 12);
			end if;			
		end if;

		-- Latch frame number on SOF token
		if (usb_pid = x"5") and (usb_bit_cnt = X"1B") then
			usb_frame <= usb_shift_reg(15 downto 5);
		end if;

		-- Check CRCs at end of packet (depends on packet type)
		if usb_eop = '1' then
			if (usb_crc5 = "01100") or (usb_crc16 = x"800D") then
				usb_crc_ok <= '1';
			end if;
		else
				usb_crc_ok <= '0';
		end if;

		-- Count number of SOF packet since reset
		-- Count number of SETUP packet since reset
		-- Count number of non SOF packet since last SETUP packet
		if (usb_cnt(2 downto 0) = 5) and usb_bit_cnt = X"10" then
			if usb_pid = x"5" then 
				usb_sof_cnt <= usb_sof_cnt + 1;
			elsif usb_pid = x"D" then 
				usb_setup_cnt <= usb_setup_cnt + 1;
				usb_msg_cnt <= (others => '0');
			else
				usb_msg_cnt <= usb_msg_cnt + 1;
			end if;
		end if;

	end if;
end process;

-- Latch data to be sent to uart for display
process (clock_usb)
begin
	
	if rising_edge(clock_usb) then
		if new_data_cnt = 14 then -- (MUST wait up to 14 to get usb_eop)

		   -- Set transfer flag only for selected data 
			data_for_uart(17) <= get_data;
			-- Keep trace of end of packet
			data_for_uart(16) <= usb_eop;
			
			-- Convert low nibble to HEX ASCII
			if usb_data(3 downto 0) < x"A" then 
				data_for_uart(7 downto 0) <= (x"0"&usb_data(3 downto 0)) + x"30";
			else
				data_for_uart(7 downto 0) <= (x"0"&usb_data(3 downto 0)) + x"41" - x"0A";
			end if;

			-- Convert high nibble to HEX ASCII
			if usb_data(7 downto 4) < x"A" then 
				data_for_uart(15 downto 8) <= (x"0"&usb_data(7 downto 4)) + x"30";
			else
				data_for_uart(15 downto 8) <= (x"0"&usb_data(7 downto 4)) + x"41" - x"0A";
			end if;
			
		end if;
		
		-- Release transfert flag
		-- Set to 1 from fast counter = 14 to 20 to allow clock domain crossing)
		if new_data_cnt = 20 then
			data_for_uart(17) <= '0';
		end if;

	end if;
end process;

-- Jtag UART from QSYS (8k fifo from avalon side to JTAG interface)
u0 : component jtag_uart_8kw
port map (
	av_chipselect  => uart_chipselect,         --    av.chipselect
	av_address     => uart_address,            --      .address
	av_read_n      => uart_read_n,             --      .read_n
	av_readdata    => uart_readdata,           --      .readdata
	av_write_n     => uart_write_n,            --      .write_n
	av_writedata   => uart_writedata,          --      .writedata
	av_waitrequest => uart_waitrequest,        --      .waitrequest
	clk_clk        => clock_30,
	irq_irq        => uart_irq,
	reset_reset_n  => reset_n
);

-- select data to be transfered w.r.t user commands
get_data <=
	'0' when (usb_bit_cnt < 16) and (usb_eop = '0') else -- sync bytes
	'0' when line_cnt = "11"&x"FF" else -- max line reached
	'0' when (want_nothing = '1')  else
	'1' when (want_sof     = '1') and ( usb_pid  = x"5") else
	'1' when (want_data    = '1') and ((usb_pid  = x"B") or (usb_pid   = x"3")) else
	'1' when (want_setup   = '1') and ( usb_pid  = x"D") else
	'1' when (want_token   = '1') and ((usb_pid /= x"B") and (usb_pid /= x"3") and (usb_pid /= x"D")) else
	'0';
	
-- Jtag UART management	
process (reset, clock_30)
begin
	
	if rising_edge(clock_30) then
	
		-- Reset line counter on usb bus reset command
		-- (allow capturing setup packets)
		if key(1) = '0' then line_cnt <= (others => '0'); end if; 
		
		-- Write sequence (3 stages for each byte to be written)
		case write_seq is
			when "00" =>
				-- Wait for previous write has ended and new data to be written.
				-- Then request for high nibbe HEX ASCII to be written.
				if (uart_write_request = '0') and (data_for_uart(17) = '1') then
					write_seq <= write_seq + 1;			
					uart_write_request <= '1';
					uart_byte <= data_for_uart(15 downto 8);
				end if;
			when "01" =>
				-- Wait for previous write has ended.
				-- Then request for low nibbe HEX ASCII to be written.
				if uart_write_request = '0' then
					write_seq <= write_seq + 1;			
					uart_write_request <= '1';
					uart_byte <= data_for_uart(7 downto 0);
				end if;
			when "10" =>
				-- Wait for previous write has ended.
				-- Then request for space or new line to be written 
				-- depending on end of packet flag.
				if uart_write_request = '0' then
					write_seq <= write_seq + 1;			
					uart_write_request <= '1';
					if data_for_uart(16) = '0' then -- eop flag
						uart_byte <= x"20";				
					else
						uart_byte <= x"0A";
						-- Count one more line after each packet if limited
						-- number of line is required.
						-- Set to all '1' when max number of lines is reached.
						if max_line /= x"0" then 
							if line_cnt < max_line & "000000" then 
								line_cnt <= line_cnt + 1;
							else
								line_cnt <= (others => '1');						
							end if;
						end if;
						
					end if;
				end if;
			when others =>
				write_seq <= (others => '0');			
		end case;

		-- JTAG UART state machine
		case uart_stm is
			-- Wait stage
			when x"0" =>
				-- If write requested goto write stage
				-- otherwise prepare reading
				if uart_write_request = '1' then 
					uart_stm <= x"4";
				else			
					uart_chipselect <= '1';
					uart_read_n     <= '0';
					uart_stm <= uart_stm + 1;
				end if;
			-- Read stage
			when x"1" =>
				-- Wait for read ready then release read request
				if uart_waitrequest = '0' then 
					uart_chipselect <= '0';
					uart_read_n     <= '1';
					-- If data from jtag available then analyse user command
					-- Otherwise return to stage 0
					if uart_readdata(15) = '1'  then
						
						if uart_readdata(7 downto 0) = X"20" then -- space : start/stop acquisition
							want_nothing <= not want_nothing;
						end if;
						if uart_readdata(7 downto 0) = X"31" then -- 1 : toggle token packet filter
							want_token <= not want_token;
						end if;
						if uart_readdata(7 downto 0) = X"32" then -- 2 : toggle sof packet filter
							want_sof <= not want_sof;
						end if;
						if uart_readdata(7 downto 0) = X"33" then -- 3 : toggle data packet filter
							want_data <= not want_data;
						end if;
						if uart_readdata(7 downto 0) = X"34" then -- 4 : toggle setup packet filter
							want_setup <= not want_setup;
						end if;
						if uart_readdata(7 downto 0) = X"36" then -- 6 : trigger acquisition
							line_cnt <= (others => '0');
						end if;
						if uart_readdata(7 downto 0) = X"37" then -- 7 : more lines to be sent
							max_line <= max_line + 1;
						end if;
						if uart_readdata(7 downto 0) = X"38" then -- 8 : less lines to be sent
							max_line <= max_line - 1;
						end if;
												
						-- Latch user command (for 7 segments display)
						read_data <= uart_readdata(7 downto 0);
						
						-- Echo user command to jtag (loop back character to terminal)
						-- goto write stage 
						uart_byte <= uart_readdata(7 downto 0);
						uart_stm <= x"4";
					else
						uart_stm <= x"0";
					end if;					
				end if;
			-- Write stage
			when x"4" =>
				uart_writedata <= x"000000"&uart_byte;
				uart_write_request <= '0';  -- Acknowlegde write request
				uart_chipselect <= '1';
				uart_write_n    <= '0';
				uart_stm <= uart_stm + 1;
			-- Wait for end of write
			when x"5" =>
				-- Wait for write ready then release read request
				if uart_waitrequest = '0' then 
					uart_chipselect <= '0';
					uart_write_n    <= '1';
					-- Return to write stage if new write request is pending
					-- Otherwise goto stage 0
					if uart_write_request = '1' then
						uart_stm <= x"4";
					else
						uart_stm <= x"0";
					end if;
				end if;
				
			when others =>
					uart_stm <= x"0";
					
		end case;

	end if;
end process;

-- 7 segments display
h0 : entity work.decodeur_7_seg port map(read_data(3 downto 0),hex0); -- user command
h1 : entity work.decodeur_7_seg port map(read_data(7 downto 4),hex1); -- user command
h2 : entity work.decodeur_7_seg port map(usb_frame(7 downto 4),hex2);      -- usb bus frame counter
h3 : entity work.decodeur_7_seg port map('0'&usb_frame(10 downto 8),hex3); -- usb bus frame counter
h4 : entity work.decodeur_7_seg port map(max_line,hex4); -- max line number
hex5(0) <= not (want_token and not(want_nothing)); -- token filter state
hex5(1) <= not (want_sof   and not(want_nothing)); -- sof   filter state
hex5(2) <= not (want_data  and not(want_nothing)); -- data  filter state
hex5(3) <= not (want_setup and not(want_nothing)); -- setup filter state
hex5(7 downto 4) <= "1111";

ledr(0) <= usb_crc_ok;

end struct;
