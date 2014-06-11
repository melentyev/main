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
    public class Link : Drawable
    {
        private Element mFrom, mTo;
        public int preparedFrom, preparedTo;
        public static Link ConnectingObject = null;
        public bool[] Signals = null;
        public Link(Element from = null, Element to = null, int width = 1)
            : base()
        {
            From = from;
            To = to;
            Signals = new bool[width];
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
                        Text = this.Name,
                        Visible = true,
                        Tag = this,
                        BorderStyle = BorderStyle.FixedSingle,
                        Top = (mFrom != null) ? mFrom.UIRepresentaion.Top : 50,
                        Left = (mFrom != null) ? mFrom.UIRepresentaion.Right + 30 : 30,
                        Width = 40,
                        ContextMenu = this.CreateContextMenu(new MenuItem("Connect", (o, e) =>
                            {
                                MainForm.Instance.Cursor = Cursors.Cross;
                                Link.ConnectingObject = this;
                                MainForm.Instance.Capture = true;
                            }) { Index = 0 })
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
            Func<Pen> pen = () => {
                int signalsCnt = 0;
                foreach (var s in Signals) 
                { 
                    if (s) signalsCnt++; 
                }
                return new Pen(signalsCnt == 0 ? Color.Black 
                             : signalsCnt < Signals.Length ? Color.Pink 
                             : Color.Red, Signals.Length == 1 ? 2.0f : 5.0f);
            };
            if (mFrom != null && mTo == null)
            {
                if (mFrom.UIRepresentaion != UIRepresentaion) 
                {
                    var p1 = new Point(mFrom.UIRepresentaion.Right, mFrom.UIRepresentaion.Top + 5);
                    var p2 = new Point(UIRepresentaion.Left, UIRepresentaion.Top + 5);
                    g.DrawLine(pen(), p1, p2);
                }
            }
            else if (mTo != null && mFrom != null)
            {
                if (mFrom.UIRepresentaion != mTo.UIRepresentaion)
                {
                    var p1 = new Point(mFrom.UIRepresentaion.Right, mFrom.UIRepresentaion.Top + 5);
                    var p2 = new Point(mTo.UIRepresentaion.Left, mTo.UIRepresentaion.Top + 5);
                    g.DrawLine(pen(), p1, p2);
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
        public void ChangeSignalTo(bool newSignal)
        {
            ChangeSignalTo(new bool[] { newSignal });
        }
        public void ChangeSignalTo(IEnumerable<bool> newSignals) {
            int i = 0;
            bool changed = false;
            foreach (var newSignal in newSignals)
            {
                if (Signals[i] != newSignal)
                {
                    Signals[i] = newSignal;
                    changed = true;
                }
                i++;
            }
            if (To != null && changed)
            {
                To.SignalChanged(this);
            }
        }       
        protected Link(SerializationInfo info, StreamingContext context)
            : base(info, context)
        {
            this.Signals = new bool[info.GetInt32("signals_count")];
            this.preparedFrom = info.GetInt32("from");
            this.preparedTo = info.GetInt32("to");
            this.From = null;
            this.To = null;
        }

        public override void GetObjectData(SerializationInfo info,  StreamingContext context)
        {
            base.GetObjectData(info, context);
            preparedFrom = this.From == null ? -1 : this.From.Id;
            preparedTo = this.To == null ? -1 : this.To.Id;
            info.AddValue("from", preparedFrom);
            info.AddValue("to", preparedTo);
            info.AddValue("signals_count", Signals.Length);
        }
    }
}
