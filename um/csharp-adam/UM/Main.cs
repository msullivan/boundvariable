// project created on 4/9/2006 at 11:21 PM
using System;
using System.IO;
using System.Collections;

namespace UM
{
	class UM
	{
		public static void Main(string[] args)
		{
			if (args.Length != 1) {
				Console.WriteLine("usage: program.um");
				return;
			}
			
			// read the file
			FileStream f = new FileStream(args[0], FileMode.Open);
			
			byte[] data = new byte[4];
			IList program = new ArrayList();
			
			while (f.Read(data, 0, 4) == 4) {
				uint word = (uint) ((data[0] << 24) | (data[1] << 16) | (data[2] << 8) | data[3]);
				program.Add(word);
			}
		
		
			// done reading
			uint[] progArray = new uint[program.Count];
			program.CopyTo(progArray, 0);
			Interpreter um = new Interpreter(progArray);
			
			um.Run();
		}
	}
}