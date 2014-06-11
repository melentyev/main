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
    public class VirtualCheckboxElement : Element, IDisposable
    {
        public override string Name
        {
            get
            {
                return base.Name;
            }
            set
            {
                base.Name = value;
                if (this.UIRepresentaion != null)
                {
                    this.UIRepresentaion.Width = 26 + this.Name.Length * 9;
                }
            }
        }
        public int Order = 0;
        public override void PrepareForUI(Point pos)
        {
            if (PositionBase != null) { 
                PositionBase.PositionChildrens.Add(this);
            }
            this.UIRepresentaion = new CheckBox()
            {
                Text = this.Name,
                Visible = true,
                Tag = this,
                Location = PositionBase.UIRepresentaion.Location + new Size(-40, Order * 25),
                Width = 26 + this.Name.Length * 9,
                ContextMenu = this.CreateContextMenu(),
            };
            this.UIRepresentaion.MouseDown += Element.UIRepresentaionMouseDown;
            MainForm.Instance.Controls.Add(this.UIRepresentaion);
            MainForm.Instance.VisibleElements.Add(this);
        }
        public VirtualCheckboxElement(Link to = null, Element PositioningParent = null, string name = "", int order = 0)
            : base()
        {
            this.Name = name;
            this.Order = order;
            if (to == null)
            {
                to = new Link();
            }
            to.From = this;
            this.outputs = new Link[1] { to };
            PositionBase = PositioningParent;
            this.PrepareForUI(new Point(0, 0));
        }
        public override void SignalChanged(Link sender)
        {
            outputs[0].ChangeSignalTo(outputs[0].Signals.Select(s => !s).ToArray());
        }
        public override void DrawSelf(Graphics g)
        {
            outputs[0].DrawSelf(g);
        }
        protected VirtualCheckboxElement(SerializationInfo info, StreamingContext context) 
            : base(info, context)
        {
            
        }
        public void Dispose()
        {
            if (UIRepresentaion != null)
            {
                UIRepresentaion.Dispose();
            }
        }
    }
}
