using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace ConsoleApplication1
{
    class Program
    {
        static void Main(string[] args)
        {
            string id = ASCOM.DriverAccess.Camera.Choose("");
        }
    }
}
