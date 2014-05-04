using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;
using System.Drawing;

namespace CtorShit
{
    public class ElementNOT : Element
    {
        static readonly Point DefaultPosition = new Point(30, 50);
        public ElementNOT(Link In = null, Link Out = null, Point? pos = null)
        {
            if (pos == null)
            {
                pos = DefaultPosition;
            }
            this.UIRepresentaion = new Label()
            {
                Text = "NOT",
                Visible = true,
                Tag = this,
                BorderStyle = BorderStyle.FixedSingle,
                Location = pos.Value,
                Width = 40,
                Height = 25,
            };
            this.UIRepresentaion.MouseDown += Element.UIRepresentaionMouseDown;
            mainForm.Controls.Add(this.UIRepresentaion);
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
            if (inputs.Length > 0 && outputs.Length > 0)
            {
                outputs[0].Signal = !inputs[0].Signal;
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
