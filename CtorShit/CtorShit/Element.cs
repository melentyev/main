using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Drawing;
using System.Windows.Forms;
using System.Threading.Tasks;

namespace CtorShit
{
    public abstract class Element : Drawable
    {
        public Link[] inputs = new Link[0];
        public Link[] outputs = new Link[0];
        public Element PositionBase = null;
        public List<Element> PositionChildrens = new List<Element>();
        public static MainForm mainForm = null;
        public virtual void SignalChanged(Link sender)
        {
            foreach (var output in this.outputs)
            {
                if (output.To != null)
                {
                    output.To.SignalChanged(output);
                }
            }
        }
        public static void UIRepresentaionMouseDown(object sender, MouseEventArgs e)
        {
            if (mainForm.PlacingElementNow)
            {
                return;
            }
            Drawable el = (sender as Control).Tag as Drawable;
            if (e.Button == MouseButtons.Left ) 
            {
                if (el is VirtualCheckboxElement)
                {
                    ((CheckBox)sender).Checked = !(((CheckBox)sender).Checked);
                    ((VirtualCheckboxElement)el).SignalChanged(null);
                    mainForm.Invalidate();
                    mainForm.Cursor = Cursors.Hand;
                }
                Element.MovingObject = el;
                el.MovingPreviousLocation = e.Location + ((Size)el.UIRepresentaion.Location);
                mainForm.Capture = true;
            }
            if (el is Link && e.Button == MouseButtons.Right)
            {
                mainForm.Cursor = Cursors.Cross;
                Link.ConnectingObject = (Link)el;
                mainForm.Capture = true;
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
                        foreach (var lnk in ((Element)el).inputs.Concat(((Element)el).outputs))
                        {
                            lnk.RepositonChildrens(delta, (Element)el);
                        }
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
                mainForm.Cursor = Cursors.Default;
                el.DrawSelf(mainForm.CreateGraphics());
            }
            if (e.Button == MouseButtons.Right && Link.ConnectingObject != null) 
            {
                Link lnk = Link.ConnectingObject;
                Link.ConnectingObject = null;
                mainForm.Cursor = Cursors.Default;
                Control ctrl = mainForm.GetChildAtPoint(e.Location);
                if (ctrl != null && ctrl.Tag is Element)
                {
                    if (ctrl.Tag is VirtualCheckboxElement)
                    {
                        VirtualCheckboxElement cb = (VirtualCheckboxElement)ctrl.Tag;
                        lnk.To = cb.outputs[0].To;
                        lnk.To.inputs[Array.FindIndex(lnk.To.inputs, (link) => link == cb.outputs[0])] = lnk;
                        cb.Dispose();
                        lnk.To.SignalChanged(lnk);
                        mainForm.VisibleElements.Remove(cb);
                    }
                    else
                    {
                        lnk.To = (Element)ctrl.Tag;
                    }
                    mainForm.Invalidate();
                }
            }
        }
    }
}
