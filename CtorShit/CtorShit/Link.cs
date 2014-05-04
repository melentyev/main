using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;
using System.Drawing;

namespace CtorShit
{
    public class Link : Drawable
    {
        private Element mFrom, mTo;
        public static Link ConnectingObject = null;
        public bool Signal = false;
        public Link(Element from = null, Element to = null)
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
                    Element.mainForm.Controls.Remove(UIRepresentaion);
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
                    Element.mainForm.Controls.Add(UIRepresentaion);
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
                var p1 = new Point(mFrom.UIRepresentaion.Right, mFrom.UIRepresentaion.Top + 5);
                var p2 = new Point(UIRepresentaion.Left, UIRepresentaion.Top + 5);
                var pen = new Pen(Signal ? Color.Red : Color.Black, 2.0f);
                g.DrawLine(pen, p1, p2);
            }
            else if (mTo != null && mFrom != null)
            {
                var p1 = new Point(mFrom.UIRepresentaion.Right, mFrom.UIRepresentaion.Top + 5);
                var p2 = new Point(mTo.UIRepresentaion.Left, mTo.UIRepresentaion.Top + 5);
                var pen = new Pen(Signal ? Color.Red : Color.Black, 2.0f);
                g.DrawLine(pen, p1, p2);
            }
        }
        public void RepositonChildrens(Size delta, Element parent)
        {
            if (mTo != null && mTo.PositionBase == parent)
            {
                mTo.UIRepresentaion.Location += delta;
            }
            else if (mFrom != null && mFrom.PositionBase == parent)
            {
                mFrom.UIRepresentaion.Location += delta;
            }
        }
    }
}
