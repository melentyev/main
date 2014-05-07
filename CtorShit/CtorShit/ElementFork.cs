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
        public ElementFork(Link In = null, Link[] Outs = null, Point? pos = null, bool forSaving = false)
            : base()
        {
            if (!forSaving)
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
                    Width = 15,
                    Height = 15,
                };
                this.UIRepresentaion.MouseDown += Element.UIRepresentaionMouseDown;
                MainForm.Instance.Controls.Add(this.UIRepresentaion);
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
            }
            this.inputs = new Link[1] { In };
            this.outputs = Outs;
        }
        public override Element GetCopyForSaving()
        {
            return new ElementFork(null, new Link[this.outputs.Length], null, true);
        }
        public override void SignalChanged(Link sender)
        {
            if (inputs.Length > 0 && outputs.Length > 0)
            {
                foreach (var _out in outputs)
                {
                    _out.ChangeSignalTo(inputs[0].Signal);
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
