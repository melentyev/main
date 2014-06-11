using System;
using System.IO;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;
using System.Collections.Generic;
using System.Runtime.Serialization;
using System.Runtime.Serialization.Formatters.Binary;
using System.Runtime.Serialization.Formatters.Soap;
using System.Diagnostics;

namespace CtorShit
{
    public partial class MainForm : Form
    {
        public HashSet<Element> VisibleElements = new HashSet<Element>();
        public HashSet<Element> UnitingElements = new HashSet<Element>();
        public Action<Point> PlacingAction = null;
        public bool PlacingElementNow = false;
        public bool UnitingNow = false;
        private static MainForm instance;
        public static MainForm Instance 
        {
            get { return instance; }
        }
        public MainForm()
        {
            MainForm.instance = this;
            InitializeComponent();
            this.MouseMove += Element.FormMouseMove;
            this.MouseUp += Element.FormMouseUp;
            var files = Directory.GetFiles(Directory.GetCurrentDirectory());
            foreach (var path in files)
            {
                if (Path.GetExtension(path) == ".cbin")
                {
                    IFormatter formatter = new BinaryFormatter();
                    Stream fileStream = new FileStream(path, FileMode.Open, FileAccess.Read);
                    List<Element> elems = new List<Element>();
                    List<Link> links = new List<Link>();
                    var NEW = ElementCluster.DeserializeFrom(formatter, fileStream, elems, links);
                    foreach (var l in links)
                    {
                        if (l.UIRepresentaion != null)
                        {
                            this.Controls.Remove(l.UIRepresentaion);
                            l.UIRepresentaion.Dispose();
                            l.UIRepresentaion = null;
                        }
                    }
                    fileStream.Position = 0;
                    
                    var buffer = new byte[fileStream.Length];
                    fileStream.Read(buffer, 0, buffer.Length);
                    MemoryStream memStream = new MemoryStream(buffer);
                    ElementCluster.AddClusterMenuItem(NEW.Name, formatter, memStream);
                }
            }
            /*var cats = PerformanceCounterCategory.GetCategories();
            var catsb = cats.Select(c => c.CategoryName).ToArray();
            var p = cats.Where(c => c.CategoryName == "Сведения о процессоре").First().ReadCategory();
            foreach (var p1 in p.Values)
            {
                
            }*/
            using (PerformanceCounter pfc1 = new PerformanceCounter("Процессор", "% загруженности процессора", "_Total")) {
                while (true) {
                    var a = pfc1.NextValue();
                    System.Threading.Thread.Sleep(1000);
                }
            };
        }
        public string Prompt(string text, string caption, string defaultValue = "")
        {
            Form prompt = new Form();
            prompt.Width = 500;
            prompt.Height = 150;
            prompt.Text = caption;
            Label textLabel = new Label() { Left = 50, Top = 20, Text = text };
            TextBox textBox = new TextBox() { Left = 50, Top = 50, Width = 400, Text = defaultValue };
            Button confirmation = new Button() { Text = "Ok", Left = 350, Width = 100, Top = 70 };
            confirmation.Click += (sender, e) => { prompt.Close(); };
            prompt.Controls.Add(confirmation);
            prompt.Controls.Add(textLabel);
            prompt.Controls.Add(textBox);
            prompt.ShowDialog();
            return textBox.Text;
        }

        private void MainFormLoad(object sender, EventArgs e)
        {

        }
        public void PlacingBinaryElement(string name)
        {
            PlacingElementNow = true;
            Cursor = Cursors.Cross;
            PlacingAction =
                (pos) =>
                {
                    var el = typeof(ElementBinaryOp).GetMethod(name).Invoke(null, new object[]{null, null, null, pos, false}) as Element;
                    var cbA = new VirtualCheckboxElement(el.inputs[0], el, "A", 0);
                    var cbB = new VirtualCheckboxElement(el.inputs[1], el, "B", 1);
                    VisibleElements.Add(el);
                    this.Invalidate();
                };
        }

        private void exitToolStripMenuItem_Click(object sender, EventArgs e)
        {
            this.Close();
        }

        private void nOTToolStripMenuItem_Click(object sender, EventArgs e)
        {
            PlacingElementNow = true;
            Cursor = Cursors.Cross;
            PlacingAction =
                (pos) =>
            {
                var el = new ElementNOT(null, null, pos);
                var cb = new VirtualCheckboxElement(el.inputs[0], el);
                VisibleElements.Add(el);
                this.Invalidate();
            };
        }

        private void MainForm_Paint(object sender, PaintEventArgs e)
        {
            BufferedGraphicsContext currentContext;
            BufferedGraphics myBuffer;
            currentContext = BufferedGraphicsManager.Current;
            myBuffer = currentContext.Allocate(this.CreateGraphics(), this.DisplayRectangle);
            myBuffer.Graphics.FillRectangle(new SolidBrush(Color.White), this.DisplayRectangle);
            foreach (var el in VisibleElements)
            {
                el.DrawSelf(myBuffer.Graphics);
            }
            myBuffer.Render();
            myBuffer.Dispose();
        }

        private void MainForm_MouseDown(object sender, MouseEventArgs e)
        {
            if (PlacingElementNow)
            {
                PlacingElementNow = false;
                Cursor = Cursors.Default;
                if (e.Button == MouseButtons.Left)
                {
                    PlacingAction(e.Location);
                }
                else
                {
                    PlacingAction = null;
                }
            }
        }

        private void fork2ToolStripMenuItem_Click(object sender, EventArgs e)
        {
            PlacingElementNow = true;
            Cursor = Cursors.Cross;
            PlacingAction =
                (pos) =>
                {
                    var el = new ElementFork(null, new Link[2] { null, null }, pos);
                    var cb = new VirtualCheckboxElement(el.inputs[0], el);
                    //cbA.SignalChanged(null);
                    VisibleElements.Add(el);
                    this.Invalidate();
                };
        }

        

        private void generatorToolStripMenuItem_Click(object sender, EventArgs e)
        {
            PlacingElementNow = true;
            Cursor = Cursors.Cross;
            PlacingAction =
                (pos) =>
                {
                    var el = new Generator(null, pos);
                    VisibleElements.Add(el);
                    this.Invalidate();
                };
        }

        private void delayToolStripMenuItem_Click(object sender, EventArgs e)
        {
            PlacingElementNow = true;
            Cursor = Cursors.Cross;
            PlacingAction =
                (pos) =>
                {
                    var el = new ElementDelay(null, null, pos);
                    var cb = new VirtualCheckboxElement(el.inputs[0], el);
                    VisibleElements.Add(el);
                    this.Invalidate();
                };
        }

        private void uniteToolStripMenuItem_Click(object sender, EventArgs e)
        {
            if (UnitingNow) 
            {
                Cursor = Cursors.Default;
                UnitingNow = false;
                var cluster = new ElementCluster();
                foreach (var el in UnitingElements)
                {
                    VisibleElements.Remove(el);
                    el.UIRepresentaion.Dispose();
                    cluster.AddElement(el);
                }
                UnitingElements.Clear();
                VisibleElements.Add(cluster);
                this.Invalidate();
                var name = Prompt("Save Cluster", "Save Cluster");
                cluster.Name = name;
                cluster.SaveAs(name);
            }
            else 
            {
                Cursor = Cursors.Cross;
                UnitingNow = true;
            }
        }

        private void fork1ToolStripMenuItem_Click(object sender, EventArgs e)
        {
            PlacingElementNow = true;
            Cursor = Cursors.Cross;
            PlacingAction =
                (pos) =>
                {
                    var el = new ElementFork(null, new Link[1] { null, }, pos);
                    var cb = new VirtualCheckboxElement(el.inputs[0], el);
                    //cbA.SignalChanged(null);
                    VisibleElements.Add(el);
                    this.Invalidate();
                };
        }

        private void xORToolStripMenuItem_Click(object sender, EventArgs e)
        {
            PlacingBinaryElement("XOR");
        }

        private void aNDToolStripMenuItem_Click(object sender, EventArgs e)
        {

            PlacingBinaryElement("AND");
        }

        private void oRToolStripMenuItem_Click(object sender, EventArgs e)
        {
            PlacingBinaryElement("OR");
        }
        private void nANDToolStripMenuItem_Click(object sender, EventArgs e)
        {
            PlacingBinaryElement("NAND");
        }

        private void nORToolStripMenuItem_Click(object sender, EventArgs e)
        {
            PlacingBinaryElement("NOR");
        }

        private void linkUniter4ToolStripMenuItem_Click(object sender, EventArgs e)
        {
            PlacingElementNow = true;
            Cursor = Cursors.Cross;
            PlacingAction =
                (pos) =>
                {
                    var el = new ElementLinkUniter(new Link[4], pos);
                    for (int i = 0; i < 4; i++)
                    {
                        var temp = new VirtualCheckboxElement(el.inputs[i], el);
                    }
                    //cbA.SignalChanged(null);
                    VisibleElements.Add(el);
                    this.Invalidate();
                };
        }
    }
}
