using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Drawing;
using System.Windows.Forms;
using System.Runtime.Serialization;
using System.Runtime.Serialization.Formatters.Binary;


namespace CtorShit
{
    [Serializable]
    class ElementLinkUniter : Element
    {
        static readonly Point DefaultPosition = new Point(30, 50);
        public override void PrepareForUI(Point pos)
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
                Location = pos,
                Width = 35,
                Height = 25,
                ContextMenu = this.CreateContextMenu()
            };
            this.UIRepresentaion.MouseDown += Element.UIRepresentaionMouseDown;
            MainForm.Instance.Controls.Add(this.UIRepresentaion);
            for (int i = 0; i < inputs.Length; i++) 
            {
                inputs[i] = new Link(null, this);
            }
            outputs[0] = new Link(this, null, inputs.Length);            
        }
        public ElementLinkUniter(Link[] ins, Point? pos = null, bool forSaving = false)
            : base()
        {
            this.inputs = ins;
            this.outputs = new Link[1];
            if (!forSaving)
            {
                PrepareForUI(pos == null ? DefaultPosition : pos.Value);
            }
        }
        public override void SignalChanged(Link sender)
        {
            outputs[0].ChangeSignalTo(inputs.Select(l => l.Signals[0] ));
        }
        public override void DrawSelf(Graphics g)
        {
            foreach (var _out in outputs)
            {
                _out.DrawSelf(g);
            }
        }
        protected ElementLinkUniter(SerializationInfo info, StreamingContext context)
            : base(info, context)
        {
        }
        public override void GetObjectData(SerializationInfo info, StreamingContext context)
        {
            base.GetObjectData(info, context);
        }
    }
}

