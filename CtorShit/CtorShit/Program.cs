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
        private static int IdAutoIncrement = 0;
        public int Id;
        public static Drawable MovingObject = null;
        public Point MovingPreviousLocation;
        public Control UIRepresentaion = null;
        public virtual void PrepareForUI(Point? pos) { }
        public static int NewId() 
        {
            return ++Drawable.IdAutoIncrement;
        }
        public abstract void DrawSelf(Graphics g);
        public Drawable()
        {
            this.Id = Drawable.NewId();  
        }
    }

}
