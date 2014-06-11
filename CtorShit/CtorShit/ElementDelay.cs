using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;
using System.Drawing;

namespace CtorShit
{
    public class ElementDelay : Element
    {
        static readonly Point DefaultPosition = new Point(30, 50);
        static HashSet<Timer> ActiveTimers = new HashSet<Timer>();
        public int TimerInterval = 500;
        public ElementDelay(Link In = null, Link Out = null, Point? pos = null)
            : base()
        {
            if (pos == null)
            {
                pos = DefaultPosition;
            }
            this.UIRepresentaion = new Label()
            {
                Text = "Delay",
                Visible = true,
                Tag = this,
                BorderStyle = BorderStyle.FixedSingle,
                Location = pos.Value,
                Width = 45,
                Height = 25,
            };
            this.UIRepresentaion.MouseDown += Element.UIRepresentaionMouseDown;
            MainForm.Instance.Controls.Add(this.UIRepresentaion);
            if (In == null)
            {
                In = new Link(null, this);
            }
            In.To = this;
            if (Out == null)
            {
                Out = new Link(this, null);
            }
            Out.From = this;
            this.inputs = new Link[1] { In };
            this.outputs = new Link[1] { Out };
        }
        public override void SignalChanged(Link sender)
        {
            var res = DelaySignalChanged();
        }
        public async Task DelaySignalChanged()
        {
            await Task.Delay(TimerInterval);
            if (inputs.Length > 0 && outputs.Length > 0)
            {
                outputs[0].ChangeSignalTo(inputs[0].Signals);
                MainForm.Instance.Invoke(new Action(() =>
                {
                    MainForm.Instance.Invalidate();
                }));
            }
        }
        public override void DrawSelf(Graphics g)
        {
            outputs[0].DrawSelf(g);
        }
    }
}
