using System;
using System.IO;
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
    public class ElementCluster : Element
    {
        public int[] preparedClusterElements;
        public List<Element> ClusterElements = new List<Element>();
        static readonly Point DefaultPosition = new Point(30, 50);
        public override void PrepareForUI(Point pos)
        {
            base.PrepareForUI(pos);
            this.UIRepresentaion = new Label()
            {
                Text = (this.Name == "" ? "Cluster" : this.Name),
                Visible = true,
                Tag = this,
                BorderStyle = BorderStyle.FixedSingle,
                Location = pos,
                Width = 60,
                Height = 25,
                ContextMenu = this.CreateContextMenu(),
            };
            this.UIRepresentaion.MouseDown += Element.UIRepresentaionMouseDown;
            MainForm.Instance.Controls.Add(this.UIRepresentaion);
        }
        public ElementCluster(Point? pos = null, bool forSaving = false)
            : base()
        {
            if (!forSaving) 
            {
                PrepareForUI(pos == null ? DefaultPosition : pos.Value);
            }
            this.inputs = new Link[0];
            this.outputs = new Link[0];
        }
        public override void SignalChanged(Link sender){}
        public override void Remove()
        {
            base.Remove();
            foreach (var e in ClusterElements)
            {
                e.Remove();
            }
            ClusterElements.Clear();
        }
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
                foreach (var l in e.inputs)
                {
                    if (l.From is VirtualCheckboxElement)
                    {
                        if (l.From.PositionBase != null)
                        {
                            l.From.PositionBase = this;
                            this.PositionChildrens.Add(l.From);
                        }
                    }
                }
            }
        }
        protected ElementCluster(SerializationInfo info, StreamingContext context) 
            : base(info, context) 
        { 
            preparedClusterElements = (int[])info.GetValue("ClusterElements", typeof(int[]));
        }
        public override void GetObjectData(SerializationInfo info, StreamingContext context)
        {
            base.GetObjectData(info, context);
            preparedClusterElements = this.ClusterElements.Select(e => e.Id).ToArray();
            info.AddValue("ClusterElements", preparedClusterElements);
        }
        public void SerializeTo(IFormatter formatter, Stream stream)
        {
            formatter.Serialize(stream, this); 
            var links = new HashSet<Link>();
            foreach (var el in this.ClusterElements)
            {
                formatter.Serialize(stream, el);
                foreach (var l in el.inputs.Concat(el.outputs)) 
                { 
                    links.Add(l);
                }
            }
            foreach (var l in links)
            {
                formatter.Serialize(stream, l);
                if (l.From is VirtualCheckboxElement)
                {
                    formatter.Serialize(stream, l.From);
                }
            }
            stream.Flush();
        }
        public static ElementCluster DeserializeFrom(IFormatter formatter, Stream stream, List<Element> elems, List<Link> links)
        {
            ElementCluster res = null;
            try
            {
                while (stream.CanRead)
                {
                    object obj = formatter.Deserialize(stream);
                    if (obj is ElementCluster) res = (ElementCluster)obj;
                    if (obj is Link) links.Add((Link)obj);
                    else if (obj is Element) elems.Add((Element)obj);
                }
            }
            catch (System.Runtime.Serialization.SerializationException) {}
            finally { }
            return res;
        }
        public void Awake(List<Element> elems)
        {
            this.PrepareForUI(DefaultPosition);
            Array.ForEach(this.preparedClusterElements, (id) => {
                this.AddElement(elems.Find(e => e.Id == id));
            });
        }
        public static void AddClusterMenuItem(string name, IFormatter formatter, MemoryStream stream)
        {
            var newItem = new ToolStripMenuItem() { Text = name };
            newItem.Click += (o, e) =>
            {
                List<Element> elems = new List<Element>();
                List<Link> links = new List<Link>();
                stream.Position = 0;
                var NEW = ElementCluster.DeserializeFrom(formatter, stream, elems, links);
                NEW.Awake(elems);
                Element.AwakeElements(elems.ToArray(), links.ToArray());
                MainForm.Instance.VisibleElements.Add(NEW);
            };
            MainForm.Instance.myClustersToolStripMenuItem.DropDown.Items.Add(newItem);
        }
        public void SaveAs(string name)
        {
            IFormatter formatter = new BinaryFormatter();
            MemoryStream stream = new MemoryStream();
            this.SerializeTo(formatter, stream);
            ElementCluster.AddClusterMenuItem(name, formatter, stream);
            stream.Position = 0;
            var buffer = new byte[stream.Length];
            stream.Read(buffer, 0, buffer.Length);
            File.WriteAllBytes("cl_" + DateTime.Now.ToString("HH_mm_ss") + name + ".cbin", buffer);
        }
    }
}