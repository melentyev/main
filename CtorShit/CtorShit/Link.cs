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
    public class Link : Drawable, ISerializable
    {
        private Element mFrom, mTo;
        public int preparedFrom, preparedTo;
        public static Link ConnectingObject = null;
        public bool Signal = false;
        public Link(Element from = null, Element to = null)
            : base()
        {
            From = from;
            To = to;
        }
        public Element To
        {
            get { return mTo; }
            set
            {
                if (value != null && UIRepresentaion != null)
                {
                    MainForm.Instance.Controls.Remove(UIRepresentaion);
                    UIRepresentaion = null;
                }
                mTo = value;
                if (mTo == null)
                {
                    UIRepresentaion = new Label()
                    {
                        Text = "O",
                        Visible = true,
                        Tag = this,
                        BorderStyle = BorderStyle.FixedSingle,
                        Top = (mFrom != null) ? mFrom.UIRepresentaion.Top : 50,
                        Left = (mFrom != null) ? mFrom.UIRepresentaion.Right + 30 : 30,
                        Width = 20
                    };
                    UIRepresentaion.MouseDown += Element.UIRepresentaionMouseDown;
                    MainForm.Instance.Controls.Add(UIRepresentaion);
                }
            }
        }
        public Element From
        {
            get { return mFrom; }
            set { mFrom = value; }
        }
        public override void DrawSelf(Graphics g)
        {
            if (mFrom != null && mTo == null)
            {
                if (mFrom.UIRepresentaion != UIRepresentaion) 
                {
                    var p1 = new Point(mFrom.UIRepresentaion.Right, mFrom.UIRepresentaion.Top + 5);
                    var p2 = new Point(UIRepresentaion.Left, UIRepresentaion.Top + 5);
                    var pen = new Pen(Signal ? Color.Red : Color.Black, 2.0f);
                    g.DrawLine(pen, p1, p2);
                }
            }
            else if (mTo != null && mFrom != null)
            {
                if (mFrom.UIRepresentaion != mTo.UIRepresentaion)
                {
                    var p1 = new Point(mFrom.UIRepresentaion.Right, mFrom.UIRepresentaion.Top + 5);
                    var p2 = new Point(mTo.UIRepresentaion.Left, mTo.UIRepresentaion.Top + 5);
                    var pen = new Pen(Signal ? Color.Red : Color.Black, 2.0f);
                    g.DrawLine(pen, p1, p2);
                }
            }
        }
        /*public void RepositonChildrens(Size delta, Element parent)
        {
            if (mTo != null && mTo.PositionBase == parent)
            {
                mTo.UIRepresentaion.Location += delta;
            }
            else if (mFrom != null && mFrom.PositionBase == parent)
            {
                mFrom.UIRepresentaion.Location += delta;
            }
        }*/
        public void ChangeSignalTo(bool newSignal) {
            if (Signal != newSignal) {
                Signal = newSignal;
                if (To != null)
                {
                    To.SignalChanged(this);
                }
            }
        }       
        protected Link(SerializationInfo info, StreamingContext context) : base()
        {
            Id = info.GetInt32("Id");
        }

        public virtual void GetObjectData(SerializationInfo info,  StreamingContext context)
        {
            preparedFrom = this.From == null ? -1 : this.From.Id;
            preparedTo = this.To == null ? -1 : this.To.Id;
            info.AddValue("Id", this.Id);
            info.AddValue("inputs", preparedFrom);
            info.AddValue("outputs", preparedTo);
        }
    }
}
