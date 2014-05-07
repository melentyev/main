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
    public class ElementBinaryOp : Element
    {
        public string preparedOpName;
        static readonly Point DefaultPosition = new Point(30, 50);
        public Func<bool, bool, bool> SignalFunc = null;
        string Label;
        public static ElementBinaryOp AND(Link InA = null, Link InB = null, 
                Link Out = null, Point? pos = null, bool forSaving = false)
        {
            return new ElementBinaryOp(InA, InB, Out, pos, forSaving, "AND", (A, B) => A && B, "AND");
        }
        public static ElementBinaryOp OR(Link InA = null, Link InB = null,
                Link Out = null, Point? pos = null, bool forSaving = false)
        {
            return new ElementBinaryOp(InA, InB, Out, pos, forSaving, "OR", (A, B) => A || B, "OR");
        }
        public static ElementBinaryOp NAND(Link InA = null, Link InB = null,
                Link Out = null, Point? pos = null, bool forSaving = false)
        {
            return new ElementBinaryOp(InA, InB, Out, pos, forSaving, "NAND", (A, B) => !(A && B), "NAND");
        }
        public static ElementBinaryOp NOR(Link InA = null, Link InB = null,
                Link Out = null, Point? pos = null, bool forSaving = false)
        {
            return new ElementBinaryOp(InA, InB, Out, pos, forSaving, "NOR", (A, B) => !(A || B), "NOR");
        }
        public static ElementBinaryOp XOR(Link InA = null, Link InB = null,
                Link Out = null, Point? pos = null, bool forSaving = false)
        {
            return new ElementBinaryOp(InA, InB, Out, pos, forSaving, "XOR", (A, B) => (A && !B) || (!A && B), "XOR");
        }
        public override void PrepareForUI(Point? pos)
        {
            base.PrepareForUI(pos);
            if (pos == null)
            {
                pos = DefaultPosition;
            }
            if (this.UIRepresentaion == null)
            {
                this.UIRepresentaion = new Label()
                {
                    Text = this.Label,
                    Visible = true,
                    Tag = this,
                    BorderStyle = BorderStyle.FixedSingle,
                    Location = pos.Value,
                    Width = 35,
                    Height = 25,
                };
                this.UIRepresentaion.MouseDown += Element.UIRepresentaionMouseDown;
                MainForm.Instance.Controls.Add(this.UIRepresentaion);
            }
            if (this.inputs[0] == null)
            {
                this.inputs[0] = new Link(null, this);
            }
            this.inputs[0].To = this;
            if (this.inputs[1] == null)
            {
                this.inputs[1] = new Link(null, this);
            }
            this.inputs[1].To = this;
            if (this.outputs[0] == null)
            {
                this.outputs[0] = new Link(this, null);
            }
            this.outputs[0].From = this;
        }
        public ElementBinaryOp(Link InA, Link InB, Link Out,
            Point? pos, bool forSaving, string label,  Func<bool, bool, bool> signalFunc, string _preparedOpName)
            : base()
        {
            Label = label;
            SignalFunc = signalFunc;
            this.preparedOpName = _preparedOpName;
            this.inputs = new Link[2] { InA, InB };
            this.outputs = new Link[1] { Out };
            if (!forSaving)
            {
                this.PrepareForUI(pos);
            }
        }

        protected ElementBinaryOp(SerializationInfo info, StreamingContext context)
            : base(info, context)
        {
            
        }
        public override void GetObjectData(SerializationInfo info, StreamingContext context)
        {
            info.AddValue("OpName", this.preparedOpName);
        }
        public override Element GetCopyForSaving()
        {
            return new ElementBinaryOp(null, null, null, null, true, "", SignalFunc, preparedOpName);
        }
        public override void SignalChanged(Link sender)
        {
            if (inputs.Length > 1 && inputs[0] != null && inputs[1] != null
                && outputs.Length > 0 && outputs[0] != null)
            {
                outputs[0].ChangeSignalTo(SignalFunc(inputs[0].Signal, inputs[1].Signal) );
            }
        }
        public override void DrawSelf(Graphics g)
        {
            outputs[0].DrawSelf(g);
        }
    }
}



