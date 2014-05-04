using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using System.Windows.Forms;
using System.Drawing;

namespace CtorShit
{
    static class Program
    {
        /// <summary>
        /// The main entry point for the application.
        /// </summary>
        [STAThread]
        static void Main()
        {
            Application.EnableVisualStyles();
            Application.SetCompatibleTextRenderingDefault(false);
            Application.Run(new MainForm());
        }
    }
    public abstract class Drawable
    {
        public static Drawable MovingObject = null;
        public Point MovingPreviousLocation;
        public Control UIRepresentaion = null;
        public abstract void DrawSelf(Graphics g);
    }

}
