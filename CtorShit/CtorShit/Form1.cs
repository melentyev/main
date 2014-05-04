using System;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;
using System.Collections.Generic;

namespace CtorShit
{
    public partial class MainForm : Form
    {
        public List<Element> VisibleElements = new List<Element>();
        public Action<Point> PlacingAction = null;
        public bool PlacingElementNow = false;
        public MainForm()
        {
            Element.mainForm = this;
            InitializeComponent();
            this.MouseMove += Element.FormMouseMove;
            this.MouseUp += Element.FormMouseUp;
        }

        private void MainFormLoad(object sender, EventArgs e)
        {

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

        private void nANDToolStripMenuItem_Click(object sender, EventArgs e)
        {
            PlacingElementNow = true;
            Cursor = Cursors.Cross;
            PlacingAction =
                (pos) =>
                {
                    var el = new ElementNAND(null, null, null, pos);
                    var cbA = new VirtualCheckboxElement(el.inputs[0], el);
                    var cbB = new VirtualCheckboxElement(el.inputs[1], el);
                    //cbA.SignalChanged(null);
                    VisibleElements.AddRange(new Element[] { cbA, cbB, el });
                    this.Invalidate();
                };
        }

        private void nORToolStripMenuItem_Click(object sender, EventArgs e)
        {
            PlacingElementNow = true;
            Cursor = Cursors.Cross;
            PlacingAction =
                (pos) =>
                {
                    var el = new ElementNOR(null, null, null, pos);
                    var cbA = new VirtualCheckboxElement(el.inputs[0], el);
                    var cbB = new VirtualCheckboxElement(el.inputs[1], el);
                    //cbA.SignalChanged(null);
                    VisibleElements.AddRange(new Element[] { cbA, cbB, el });
                    this.Invalidate();
                };
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
    }
}
