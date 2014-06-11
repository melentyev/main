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
    public class ElementBinaryOp : Element
    {
        public string preparedOpName;
        static readonly Point DefaultPosition = new Point(30, 50);
        public Func<bool, bool, bool> SignalFunc = null;
        public static Dictionary<string, Func<bool, bool, bool>> Functions = new Dictionary<string, Func<bool, bool, bool>>();
        static ElementBinaryOp()
        {
            Functions.Add("AND", (A, B) => A && B);
            Functions.Add("OR", (A, B) => A || B);
            Functions.Add("NAND", (A, B) => !(A && B));
            Functions.Add("NOR", (A, B) => !(A || B));
            Functions.Add("XOR", (A, B) => (A && !B) || (!A && B));
        }
        public static ElementBinaryOp AND(Link InA = null, Link InB = null, 
                Link Out = null, Point? pos = null, bool forSaving = false)
        {
            return new ElementBinaryOp(InA, InB, Out, pos, forSaving, "AND", Functions["AND"], "AND");
        }
        public static ElementBinaryOp OR(Link InA = null, Link InB = null,
                Link Out = null, Point? pos = null, bool forSaving = false)
        {
            return new ElementBinaryOp(InA, InB, Out, pos, forSaving, "OR", Functions["OR"], "OR");
        }
        public static ElementBinaryOp NAND(Link InA = null, Link InB = null,
                Link Out = null, Point? pos = null, bool forSaving = false)
        {
            return new ElementBinaryOp(InA, InB, Out, pos, forSaving, "NAND", Functions["NAND"], "NAND");
        }
        public static ElementBinaryOp NOR(Link InA = null, Link InB = null,
                Link Out = null, Point? pos = null, bool forSaving = false)
        {
            return new ElementBinaryOp(InA, InB, Out, pos, forSaving, "NOR", Functions["NOR"], "NOR");
        }
        public static ElementBinaryOp XOR(Link InA = null, Link InB = null,
                Link Out = null, Point? pos = null, bool forSaving = false)
        {
            return new ElementBinaryOp(InA, InB, Out, pos, forSaving, "XOR", Functions["XOR"], "XOR");
        }
        public override void PrepareForUI(Point pos)
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
                    Text = this.Name,
                    Visible = true,
                    Tag = this,
                    BorderStyle = BorderStyle.FixedSingle,
                    Location = pos,
                    Width = 45,
                    Height = 25,
                    ContextMenu = this.CreateContextMenu(),
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
            Point? pos, bool forSaving, string name,  Func<bool, bool, bool> signalFunc, string _preparedOpName)
            : base()
        {
            Name = name;
            SignalFunc = signalFunc;
            this.preparedOpName = _preparedOpName;
            this.inputs = new Link[2] { InA, InB };
            this.outputs = new Link[1] { Out };
            if (!forSaving)
            {
                this.PrepareForUI(pos == null ? DefaultPosition : pos.Value);
            }
        }

        protected ElementBinaryOp(SerializationInfo info, StreamingContext context)
            : base(info, context)
        {
            this.preparedOpName = info.GetString("OpName");
            SignalFunc = Functions[this.preparedOpName];
        }
        public override void GetObjectData(SerializationInfo info, StreamingContext context)
        {
            base.GetObjectData(info, context);
            info.AddValue("OpName", this.preparedOpName);
        }
        public override void SignalChanged(Link sender)
        {
            if (inputs.Length > 1 && inputs[0] != null && inputs[1] != null
                && outputs.Length > 0 && outputs[0] != null)
            {
                outputs[0].ChangeSignalTo(inputs[0].Signals.Zip(inputs[1].Signals, (a, b) => SignalFunc(a, b) ) );
            }
        }
        public override void DrawSelf(Graphics g)
        {
            outputs[0].DrawSelf(g);
        }
    }
}



