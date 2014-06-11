using System;
using System.IO;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Drawing;
using System.Windows.Forms;
using System.Threading.Tasks;
using System.Runtime.Serialization;
using System.Runtime.Serialization.Formatters.Binary;

namespace CtorShit
{
    [Serializable]
    public class Element : Drawable
    {
        public int[] preparedInputs;
        public int[] preparedOutputs;
        public int preparedPositionBase = -1;
        public Link[] inputs = new Link[0];
        public Link[] outputs = new Link[0];
        public IEnumerable<Link> AllLinks
        {
            get { return inputs.Concat(outputs); }
        }
        public Element PositionBase = null;
        public List<Element> PositionChildrens = new List<Element>();
        public Element() : base() {}

        public virtual void SignalChanged(Link sender){}
        public static void UIRepresentaionMouseDown(object sender, MouseEventArgs e)
        {
            if (MainForm.Instance.PlacingElementNow)
            {
                return;
            }
            Drawable el = (sender as Control).Tag as Drawable;
            if (el == null) 
            {
                return;
            }
            else if (MainForm.Instance.UnitingNow)
            {
                if (el is Element && !(el is VirtualCheckboxElement)) 
                { 
                    ((Control)sender).BackColor = Color.Aqua;
                    MainForm.Instance.UnitingElements.Add((Element)el);
                    return;
                }
                return;
            }
            if (e.Button == MouseButtons.Left ) 
            {
                if (el is VirtualCheckboxElement)
                {
                    ((CheckBox)sender).Checked = !(((CheckBox)sender).Checked);
                    ((VirtualCheckboxElement)el).SignalChanged(null);
                    MainForm.Instance.Invalidate();
                    MainForm.Instance.Cursor = Cursors.Hand;
                }
                Element.MovingObject = el;
                el.MovingPreviousLocation = e.Location + ((Size)el.UIRepresentaion.Location);
                MainForm.Instance.Capture = true;
            }
            if (el is Link && e.Button == MouseButtons.Right)
            {
                /*MainForm.Instance.Cursor = Cursors.Cross;
                Link.ConnectingObject = (Link)el;
                MainForm.Instance.Capture = true;*/
            }
        }

        public static void FormMouseMove(object sender, MouseEventArgs e)
        {
            if (Element.MovingObject != null)
            {
                Drawable el = Element.MovingObject;
                if (Element.MovingObject == el && el.MovingPreviousLocation != e.Location)
                {
                    var delta = new Size(e.X, e.Y)
                        - new Size(el.MovingPreviousLocation.X, el.MovingPreviousLocation.Y);
                    el.UIRepresentaion.Location += delta;
                    el.MovingPreviousLocation = e.Location;
                    if (el is Element)
                    {
                        foreach(var child in ((Element)el).PositionChildrens) 
                        {
                            if (child.UIRepresentaion != null)
                            {
                                child.UIRepresentaion.Location += delta;
                            }
                        }
                        /*foreach (var lnk in ((Element)el).inputs.Concat(((Element)el).outputs))
                        {
                            lnk.RepositonChildrens(delta, (Element)el);
                        }*/
                    }
                }
            }
        }

        public static void FormMouseUp(object sender, MouseEventArgs e)
        {
            if (Element.MovingObject != null)
            {
                Drawable el = Element.MovingObject;
                Element.MovingObject = null;
                MainForm.Instance.Cursor = Cursors.Default;
                el.DrawSelf(MainForm.Instance.CreateGraphics());
            }
            else if (Link.ConnectingObject != null && e.Button == MouseButtons.Left) 
            {
                Link lnk = Link.ConnectingObject;
                Link.ConnectingObject = null;
                MainForm.Instance.Cursor = Cursors.Default;
                Control ctrl = MainForm.Instance.GetChildAtPoint(e.Location);
                if (ctrl != null && ctrl.Tag is Element)
                {
                    if (ctrl.Tag is VirtualCheckboxElement)
                    {
                        VirtualCheckboxElement cb = (VirtualCheckboxElement)ctrl.Tag;
                        lnk.To = cb.outputs[0].To;
                        lnk.To.inputs[Array.FindIndex(lnk.To.inputs, (link) => link == cb.outputs[0])] = lnk;
                        cb.Dispose();
                        lnk.To.SignalChanged(lnk);
                        MainForm.Instance.VisibleElements.Remove(cb);
                    }
                    MainForm.Instance.Invalidate();
                }
            }
            else if (Link.ConnectingObject != null && e.Button == MouseButtons.Right)
            {
                Link.ConnectingObject = null;
                MainForm.Instance.Cursor = Cursors.Default;
            }
            else if (MainForm.Instance.UnitingNow) 
            {
                if(e.Button == MouseButtons.Right)
                {
                    MainForm.Instance.UnitingNow = false;
                    MainForm.Instance.Cursor = Cursors.Default;
                    foreach (var el in MainForm.Instance.UnitingElements) 
                    {
                        if (el.UIRepresentaion != null)
                        {
                            el.UIRepresentaion.BackColor = Color.FromKnownColor(KnownColor.Control);
                        }
                    }
                    MainForm.Instance.UnitingElements.Clear();
                }
            }
        }
        public override void DrawSelf(Graphics g) {}

        public virtual void Remove() 
        {
            foreach (var l in this.inputs) { l.To = null; }
            foreach (var l in this.outputs) { l.From = null; }
            if (this.UIRepresentaion != null)
            {
                MainForm.Instance.Controls.Remove(this.UIRepresentaion);
                this.UIRepresentaion.Dispose();
            }
            MainForm.Instance.VisibleElements.Remove(this);
        }

        protected Element(SerializationInfo info, StreamingContext context)
            : base(info, context)
        {
            preparedInputs = (int[])info.GetValue("inputs", typeof(int[]));
            preparedOutputs = (int[])info.GetValue("outputs", typeof(int[]));
            preparedPositionBase = info.GetInt32("PositionBase");
        }

        public override void GetObjectData(SerializationInfo info,  StreamingContext context)
        {
            base.GetObjectData(info, context);
            preparedInputs = this.inputs.Select(el => el.Id).ToArray();
            preparedOutputs = this.outputs.Select(el => el.Id).ToArray();
            preparedPositionBase = (this.PositionBase == null ? -1 : this.PositionBase.Id);
            info.AddValue("inputs", preparedInputs);
            info.AddValue("outputs", preparedOutputs);
            info.AddValue("PositionBase", preparedPositionBase);
        }
        public static Stream StoreElements(Element[] elems) 
        {
            Stream stream = new MemoryStream();

            IFormatter bf = new BinaryFormatter();
            HashSet<Link> allLinks = new HashSet<Link>();
            foreach (var e in elems)
            {
                Array.ForEach(e.inputs.Concat(e.outputs).ToArray(), l => bf.Serialize(stream, l));
                bf.Serialize(stream, e);
            }
            return stream;
        }
        public static void AwakeElements(Element[] elems, Link[] links)
        {
            foreach (var el in elems)
            {
                el.inputs = el.preparedInputs.Select(id => Array.Find(links, l => l.Id == id) ).ToArray();
                el.outputs = el.preparedOutputs.Select(id => Array.Find(links, l => l.Id == id)).ToArray();
                Array.ForEach(el.inputs, l => l.To = el);
                Array.ForEach(el.outputs, l => l.From = el);
                el.PositionBase = Array.Find(elems, e => e.Id == el.preparedPositionBase);
            }
            foreach (var el in elems)
            {
                el.Id = Drawable.NewId();
                if (el is VirtualCheckboxElement)
                {
                    el.PrepareForUI(new Point(0, 0));
                }
                foreach (var l in el.inputs.Concat(el.outputs)) l.Id = Drawable.NewId();
            }
        }
        public override ContextMenu CreateContextMenu(params object[] items)
        {
            var _items = items.Select(e => e as MenuItem).Concat(new MenuItem[] { new MenuItem("Remove", (o, e) => {
                this.Remove();
            }) { Index = 110 } }).ToArray();
            return base.CreateContextMenu(_items);
            
        }
    }
}
