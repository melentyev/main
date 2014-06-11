using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;
using System.Drawing;

namespace CtorShit
{

    public class Generator : Element
    {
        private System.Windows.Forms.Timer mTimer = new Timer();
        static readonly Point DefaultPosition = new Point(30, 50);
        public Generator(Link Out = null, Point? pos = null)
            : base()
        {
            if (pos == null)
            {
                pos = DefaultPosition;
            }
            this.UIRepresentaion = new Label()
            {
                Text = "GenT",
                Visible = true,
                Tag = this,
                BorderStyle = BorderStyle.FixedSingle,
                Location = pos.Value,
                Width = 45,
                Height = 25,
            };
            this.UIRepresentaion.MouseDown += Element.UIRepresentaionMouseDown;
            MainForm.Instance.Controls.Add(this.UIRepresentaion);
            if (Out == null)
            {
                Out = new Link(this, null);
            }
            this.outputs = new Link[1] { Out };
            var contextMenu1 = new ContextMenu();
            var timerValues = new int[] { 500, 2000, 10000 };
            foreach (var val in timerValues)
            {
                var item = new MenuItem() { Index = 0, Text = "Interval " + val.ToString() };
                item.Click += (o, e) =>
                {
                    mTimer.Interval = val;
                    mTimer.Enabled = true;
                };
                contextMenu1.MenuItems.Add(item);
            }
            
            mTimer.Tick += (o, e) => 
            {
                SignalChanged(null);
                MainForm.Instance.Invalidate();
            };
            this.UIRepresentaion.ContextMenu = contextMenu1;
        }
        public override void SignalChanged(Link sender)
        {
            outputs[0].ChangeSignalTo(outputs[0].Signals.Select(s => !s));
        }
        public override void DrawSelf(Graphics g)
        {
            outputs[0].DrawSelf(g);
        }
    }
}
