﻿using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Drawing;
using System.Windows.Forms;

namespace CtorShit
{
    public class VirtualCheckboxElement : Element, IDisposable
    {
        public VirtualCheckboxElement(Link to = null, Element PositioningParent = null)
        {
            if (to == null)
            {
                to = new Link();
            }
            to.From = this;
            this.outputs = new Link[1] { to };
            PositionBase = PositioningParent;
            PositioningParent.PositionChildrens.Add(this);
            this.UIRepresentaion = new CheckBox()
            {
                Visible = true,
                Tag = this,
                Location = PositionBase.UIRepresentaion.Location + new Size(-20, 2),
                Width = 14
            };
            this.UIRepresentaion.MouseDown += Element.UIRepresentaionMouseDown;
            mainForm.Controls.Add(this.UIRepresentaion);
        }
        public override void SignalChanged(Link sender)
        {
            this.outputs[0].Signal = !this.outputs[0].Signal;
            base.SignalChanged(sender);
        }
        public override void DrawSelf(Graphics g)
        {
            outputs[0].DrawSelf(g);
        }
        public Link Link
        {
            get { return outputs[0]; }
            set { outputs[0] = value; }
        }
        public void Dispose()
        {
            if (UIRepresentaion != null)
            {
                UIRepresentaion.Dispose();
            }
        }
    }
}
