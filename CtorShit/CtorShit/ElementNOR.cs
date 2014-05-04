using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;
using System.Drawing;

namespace CtorShit
{
    public class ElementNOR : Element
    {
        static readonly Point DefaultPosition = new Point(30, 50);
        public ElementNOR(Link InA = null, Link InB = null, Link Out = null, Point? pos = null)
        {
            if (pos == null)
            {
                pos = DefaultPosition;
            }
            this.UIRepresentaion = new Label()
            {
                Text = "NOR",
                Visible = true,
                Tag = this,
                BorderStyle = BorderStyle.FixedSingle,
                Location = pos.Value,
                Width = 45,
                Height = 25,
            };
            this.UIRepresentaion.MouseDown += Element.UIRepresentaionMouseDown;
            mainForm.Controls.Add(this.UIRepresentaion);
            if (InA == null)
            {
                InA = new Link(null, this);
            }
            InA.To = this;
            if (InB == null)
            {
                InB = new Link(null, this);
            }
            InA.To = this;
            if (Out == null)
            {
                Out = new Link(this, null);
            }
            Out.From = this;
            this.inputs = new Link[2] { InA, InB };
            this.outputs = new Link[1] { Out };
        }
        public override void SignalChanged(Link sender)
        {
            if (inputs.Length > 1 && inputs[0] != null && inputs[1] != null
                && outputs.Length > 0 && outputs[0] != null)
            {
                outputs[0].Signal = !(inputs[0].Signal || inputs[1].Signal);
                if (outputs[0].To != null)
                {
                    outputs[0].To.SignalChanged(outputs[0]);
                }
            }
        }
        public override void DrawSelf(Graphics g)
        {
            outputs[0].DrawSelf(g);
        }
    }
}
