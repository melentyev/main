using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Drawing;
using System.Windows.Forms;

namespace CtorShit
{
    class ElementFork : Element
    {
        static readonly Point DefaultPosition = new Point(30, 50);
        public ElementFork(Link In = null, Link[] Outs = null, Point? pos = null)
        {
            if (pos == null)
            {
                pos = DefaultPosition;
            }
            this.UIRepresentaion = new Label()
            {
                Text = "",
                Visible = true,
                Tag = this,
                BorderStyle = BorderStyle.FixedSingle,
                Location = pos.Value,
                Width = 25,
                Height = 25,
            };
            this.UIRepresentaion.MouseDown += Element.UIRepresentaionMouseDown;
            mainForm.Controls.Add(this.UIRepresentaion);
            if (In == null)
            {
                In = new Link(null, this);
            }
            In.To = this;
            for (int i = 0; i < Outs.Length; i++) 
            { 
                if (Outs[i] == null)
                {
                    Outs[i] = new Link(this, null);
                }
                Outs[i].From = this;
            }
            this.inputs = new Link[1] { In };
            this.outputs = Outs;
        }
        public override void SignalChanged(Link sender)
        {
            if (inputs.Length > 0 && outputs.Length > 0)
            {
                foreach (var _out in outputs)
                {
                    _out.Signal = inputs[0].Signal;
                    if (_out.To != null)
                    {
                        _out.To.SignalChanged(_out);
                    }
                }
            }
        }
        public override void DrawSelf(Graphics g)
        {
            foreach (var _out in outputs)
            {
                _out.DrawSelf(g);
            }
        }
    }
}
