using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;
using System.Drawing;
using System.Runtime.Serialization;
using System.Runtime.Serialization.Formatters.Binary;

namespace CtorShit
{
    [Serializable]
    public class ElementNOT : Element
    {
        static readonly Point DefaultPosition = new Point(30, 50);
        public ElementNOT(Link In = null, Link Out = null, Point? pos = null, bool forSaving = false)
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
                    Text = "NOT",
                    Visible = true,
                    Tag = this,
                    BorderStyle = BorderStyle.FixedSingle,
                    Location = pos.Value,
                    Width = 40,
                    Height = 25,
                    ContextMenu = this.CreateContextMenu()
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
            }
            this.inputs = new Link[1] { In };
            this.outputs = new Link[1] { Out };
        }
        protected ElementNOT(SerializationInfo info, StreamingContext context) : base(info, context) { }
        public override void SignalChanged(Link sender)
        {
            outputs[0].ChangeSignalTo(inputs[0].Signals.Select(s => !s));
        }
        public override void DrawSelf(Graphics g)
        {
            outputs[0].DrawSelf(g);
        }
    }
}
