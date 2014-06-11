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
    public abstract class Drawable : ISerializable
    {
        private static int IdAutoIncrement = 0;
        public int Id;
        private string mName = "";
        public virtual string Name
        {
            get
            {
                return mName;
            }
            set
            {
                mName = value;
                if (this.UIRepresentaion != null)
                {
                    this.UIRepresentaion.Text = value;
                }
            }
        }
        public static Drawable MovingObject = null;
        public Point MovingPreviousLocation;
        public Control UIRepresentaion = null;
        public virtual void PrepareForUI(Point pos) { }
        public static int NewId()
        {
            return ++Drawable.IdAutoIncrement;
        }
        public abstract void DrawSelf(Graphics g);
        public Drawable()
        {
            this.Id = Drawable.NewId();
        }
        protected Drawable(SerializationInfo info, StreamingContext context)
        {
            this.Id = info.GetInt32("Id");
            this.Name = info.GetString("Name");
        }

        public virtual void GetObjectData(SerializationInfo info, StreamingContext context)
        {
            info.AddValue("Id", this.Id);
            info.AddValue("Name", this.Name);
        }
        public ContextMenu CreateContextMenu(MenuItem[] items)
        {
            var contextMenu = new ContextMenu();
            items = items.Concat(new MenuItem[] { new MenuItem("Edit Name", (o, e) => {
                this.Name = MainForm.Instance.Prompt("Edit Name", "Name:", this.Name); }) { Index = 100 } } )
                .OrderBy(i => i.Index).ToArray();
            contextMenu.MenuItems.AddRange(items);
            return contextMenu;
        }
        public virtual ContextMenu CreateContextMenu(params object[] items)
        {
            return CreateContextMenu(items.Select(e => e as MenuItem).ToArray());
        }
    }
}
