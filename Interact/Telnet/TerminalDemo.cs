// THIS CODE AND INFORMATION ARE PROVIDED "AS IS" WITHOUT WARRANTY OF ANY
// KIND, EITHER EXPRESSED OR IMPLIED!
//
// IF YOU FIND ERRORS OR POSSIBLE IMPROVEMENTS, PLEASE LET ME KNOW.
// MAYBE TOGETHER WE CAN SOLVE THIS.
//
// YOU MAY USE THIS CODE: HOWEVER THIS GRANTS NO FUTURE RIGHTS.

using System;
using System.Threading;

namespace Telnet.Demo
{
	/// <summary>
	/// Demo for the telnet class:
	/// <p>
	/// <a href="http://www.klausbasan.de/misc/telnet/index.html">Further details</a>
	/// </p>
	/// </summary>
	public class TerminalDemo
	{
		/// <summary>
		/// The main entry point for the application.
		/// Can be used to test the programm and run it from command line.
		/// </summary>
		[STAThread]
		static void Main(string[] args) 
		{
			// DemoMSTelnetServer(args);
			// DemoRH73TelnetServer(args);
			DemoRT311Router(args);
		}

		/// <summary>
		/// Demo for a MS Telnet server
		/// </summary>
		private static void DemoMSTelnetServer(string[] args)
		{
			string f = null;
			Terminal tn = new Terminal("giga", 23, 10, 80, 40); // hostname, port, timeout [s], width, height
			tn.Connect(); // physcial connection
			do 
			{
				f = tn.WaitForString("Login");
				if (f==null)
					throw new TerminalException("No login possible");
				Console.WriteLine(tn.VirtualScreen.Hardcopy().TrimEnd());
				tn.SendResponse("telnet", true);	// send username
				f = tn.WaitForString("Password");
				if (f==null) 
					throw new TerminalException("No password prompt found");
				Console.WriteLine(tn.VirtualScreen.Hardcopy().TrimEnd());
				tn.SendResponse("telnet", true);	// send password 
				f = tn.WaitForString(">");
				if (f==null) 
					throw new TerminalException("No > prompt found");
				tn.SendResponse("dir", true);		// send dir command
				if (tn.WaitForChangedScreen())
					Console.WriteLine(tn.VirtualScreen.Hardcopy().TrimEnd());
			} while (false);
			tn.Close(); // physically close on TcpClient
			Console.WriteLine("\n\nEnter to continue ...\n");
			Console.ReadLine();
		} // Demo

		/// <summary>
		/// Demo for a Linux RedHat 7.3 telnet server
		/// </summary>
		private static void DemoRH73TelnetServer(string[] args)
		{
			string f = null;
			Terminal tn = new Terminal("10.10.20.140", 23, 10, 80, 40); // hostname, port, timeout [s], width, height
			tn.Connect(); // physcial connection
			do 
			{
				f = tn.WaitForString("Login");
				if (f==null) 
					break; // this little clumsy line is better to watch in the debugger
				Console.WriteLine(tn.VirtualScreen.Hardcopy().TrimEnd());
				tn.SendResponse("kba", true);	// send username
				f = tn.WaitForString("Password");
				if (f==null) 
					break;
				Console.WriteLine(tn.VirtualScreen.Hardcopy().TrimEnd());
				tn.SendResponse("vmware", true);	// send password 
				f = tn.WaitForString("$");			// bash
				if (f==null) 
					break;
				tn.SendResponse("df", true);		// send Shell command
				if (tn.WaitForChangedScreen())
					Console.WriteLine(tn.VirtualScreen.Hardcopy().TrimEnd());
			} while (false);
			tn.Close(); // physically close on TcpClient
			Console.WriteLine("\n\nEnter to continue ...\n");
			Console.ReadLine();
		} // Demo

		/// <summary>
		/// Demo for a RT311 Router
		/// </summary>
		private static void DemoRT311Router(string[] args)
		{
			string f = null;
			Terminal tn = new Terminal("router", 23, 10, 80, 40); // hostname, port, timeout [s], width, height
			tn.Connect(); // physcial connection
			do 
			{
				f= tn.WaitForString("Password");
				if (f==null) 
					throw new TerminalException("No password prompt found");
				Console.WriteLine(tn.VirtualScreen.Hardcopy().TrimEnd());
				tn.SendResponse("1234", true);	// send password
				f = tn.WaitForString("Enter");
				if (f==null) 
					throw new TerminalException("No 1st menu screen found");
				Console.WriteLine(tn.VirtualScreen.Hardcopy().TrimEnd());
				tn.SendResponse("24", true);	// send "24" to get to next screen 
				f = tn.WaitForString("Enter", false, 30); // String, case sensitive, timeout
				if (f==null) 
					throw new TerminalException("No 2nd menu screen found");
				Console.WriteLine(tn.VirtualScreen.Hardcopy().TrimEnd());
				tn.SendResponse("1", true);		// send "1" to get to next screen
				Console.WriteLine(tn.VirtualScreen.Hardcopy().TrimEnd());
				f = Terminal.FindIPAddress(tn.WaitForRegEx(@"WAN.+?(\d?\d?\d\.\d?\d?\d\.\d?\d?\d\.\d?\d?\d)")); // search for 1st IP-like address next to "WAN"
				if (f==null) 
					throw new TerminalException("No IP address found");
				Console.WriteLine(tn.VirtualScreen.Hardcopy().TrimEnd());
				Console.WriteLine("\n\nEXTERNAL IP " + f);
			} while (false);
			tn.SendLogout(true);
			tn.Close(); // physically close on TcpClient
			Console.WriteLine("\n\nEnter to continue ...\n");
			Console.ReadLine();
		} // Demo
	} // class
} // namespace