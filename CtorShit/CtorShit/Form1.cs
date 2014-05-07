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

namespace CtorShit
{
    public partial class MainForm : Form
    {
        public List<Element> VisibleElements = new List<Element>();
        public List<Element> UnitingElements = new List<Element>();
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
                    VisibleElements.AddRange(new Element[] { cbA, cbB, el });
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
                VisibleElements.AddRange(new Element[] { cb, el });
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
                PlacingAction(e.Location);
                PlacingElementNow = false;
                Cursor = Cursors.Default;
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
                    VisibleElements.AddRange(new Element[] { cb, el });
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
                    VisibleElements.AddRange(new Element[] { el });
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
                    VisibleElements.AddRange(new Element[] { cb, el });
                    this.Invalidate();
                };
        }

        private void uniteToolStripMenuItem_Click(object sender, EventArgs e)
        {
            if (UnitingNow) 
            {
                var el1 = UnitingElements[0];
                IFormatter formatter = new BinaryFormatter();
                Stream stream = new MemoryStream();
                formatter.Serialize(stream, el1);
                stream.Flush();
                stream.Position = 0;
                var el2 = formatter.Deserialize(stream);
                return;
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

                var dlg = new Form() { Text = "Save" };
                var txtName = new TextBox();
                var btnSave = new Button() 
                { 
                    Text = "Save", 
                    Left = dlg.Right - 100, 
                    Top = dlg.Bottom - 80 
                };
                btnSave.Click += (o, args) => {
                    cluster.SaveAs(txtName.Text);
                    dlg.Close();
                };
                dlg.Controls.Add(txtName);
                dlg.Controls.Add(btnSave);
                dlg.ShowDialog();                
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
                    VisibleElements.AddRange(new Element[] { cb, el });
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
    }
}
