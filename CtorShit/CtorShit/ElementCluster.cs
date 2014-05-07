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
    public class ElementCluster : Element
    {
        public int[] preparedClusterElements;
        public string Name = "";
        public List<Element> ClusterElements = new List<Element>();
        static readonly Point DefaultPosition = new Point(30, 50);

        public ElementCluster(Point? pos = null, bool forSaving = false)
            : base()
        {
            if (pos == null)
            {
                pos = DefaultPosition;
            }
            if (!forSaving) 
            { 
                this.UIRepresentaion = new Label()
                {
                    Text = "Cluster",
                    Visible = true,
                    Tag = this,
                    BorderStyle = BorderStyle.FixedSingle,
                    Location = pos.Value,
                    Width = 60,
                    Height = 25,
                };
                this.UIRepresentaion.MouseDown += Element.UIRepresentaionMouseDown;
                MainForm.Instance.Controls.Add(this.UIRepresentaion);
            }
            this.inputs = new Link[0];
            this.outputs = new Link[0];
        }
        public override void SignalChanged(Link sender){}
        public override void DrawSelf(Graphics g)
        {
            foreach (var e in ClusterElements)
            {
                e.DrawSelf(g);
            }
        }
        public void AddElement(Element e)
        {
            if (e is ElementCluster)
            {
                foreach (var inner in ((ElementCluster)e).ClusterElements)
                {
                    this.AddElement(inner);
                }
                ((ElementCluster)e).ClusterElements.Clear();
                MainForm.Instance.VisibleElements.Remove(e);
            }
            else { 
                ClusterElements.Add(e);
                e.UIRepresentaion = this.UIRepresentaion;
            }
        }
        public override Element GetCopyForSaving()
        {
            var NEW = new ElementCluster(null, true);
            for (int i = 0; i < ClusterElements.Count; i++)
            {
                NEW.AddElement(ClusterElements[i].GetCopyForSaving());
            }
            for (int i = 0; i < ClusterElements.Count; i++)
            {
                for (int j = 0; j < ClusterElements[i].outputs.Length; j++)
                {
                    var lnk = ClusterElements[i].outputs[j];
                    if (lnk != null && NEW.ClusterElements[i].outputs[j] == null) 
                    { 
                        var ind = ClusterElements.FindIndex(e => e == lnk.To);
                        if (ind != -1)
                        {
                            var portInd = Array.FindIndex(ClusterElements[ind].inputs, l => l == lnk);
                            NEW.ClusterElements[i].outputs[j] = new Link(
                                NEW.ClusterElements[i],
                                NEW.ClusterElements[ind]
                            );
                            NEW.ClusterElements[ind].inputs[portInd] = NEW.ClusterElements[i].outputs[j];
                        }
                    }
                }

            }
            return NEW;
        }
        public override void GetObjectData(SerializationInfo info, StreamingContext context)
        {
            info.AddValue("ClusterElements", this.ClusterElements.Select(e => e.Id).ToArray());
        }
        public void SaveAs(string name)
        {
            var savedInstance = (ElementCluster)(this.GetCopyForSaving());
            savedInstance.Name = name;
            var newItem = new ToolStripMenuItem() { Text = name };
            newItem.Click += (o, e) => {
                var NEW = savedInstance.Spawn();
                MainForm.Instance.VisibleElements.Add(NEW);
            };
            MainForm.Instance.myClustersToolStripMenuItem.DropDown.Items.Add(newItem);
        }
        public ElementCluster Spawn(Point? pos = null)
        {
            var newInstance = (ElementCluster)(this.GetCopyForSaving());
            if (pos == null)
            {
                pos = DefaultPosition;
            }
            newInstance.UIRepresentaion = new Label()
            {
                Text = this.Name,
                Visible = true,
                Tag = newInstance,
                BorderStyle = BorderStyle.FixedSingle,
                Location = pos.Value,
                Width = 65,
                Height = 25,
            };
            foreach (var el in newInstance.ClusterElements)
            {
                el.UIRepresentaion = newInstance.UIRepresentaion;
                for(int i = 0; i <  el.inputs.Length; i++)
                {
                    if (el.inputs[i] == null)
                    {
                        el.inputs[i] = new Link(null, el);
                        var cb = new VirtualCheckboxElement(el.inputs[i], newInstance);
                        MainForm.Instance.VisibleElements.Add(cb);
                    }
                }
                for (int i = 0; i < el.outputs.Length; i++)
                {
                    if (el.outputs[i] == null)
                    {
                        el.outputs[i] = new Link(el, null);
                    }
                }
            }
            newInstance.UIRepresentaion.MouseDown += Element.UIRepresentaionMouseDown;
            MainForm.Instance.Controls.Add(newInstance.UIRepresentaion);
            return newInstance;
        }
    }
}