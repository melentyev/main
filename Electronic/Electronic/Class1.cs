using System;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Collections.Generic;

namespace Electronic
{
    enum ElementaryOperation
    {
        NOT = 1,
        NOT_AND = 2,
        NOT_OR = 3,
    }

    class Edge 
    {
        public bool signal = false, flush = false;
        public Element from = null, to = null;
        public Edge(Element _from, bool _signal, Element _to)
        {
            to = _from;
            to = _to;
            signal = _signal;
        }
        public bool push(bool newSignal)
        {
            bool old = signal;
            signal = newSignal;
            if (newSignal != old)
            {
                Element.nextQueue.Add(this);
            }
            return (flush = (newSignal != old) );
        }
    }

    class Element
    {
        List<Edge> listInput, listOutput;
        public static SortedSet<Edge> queue, nextQueue;  
        static List<Element> types;
        bool isElementary = false;
        ElementaryOperation elementaryOpertaion;
        Element Type;
        Element(bool _isElementary, ElementaryOperation op) 
        {
            isElementary = _isElementary;
            elementaryOpertaion = op;
            listInput = new List<Edge>();
            listOutput = new List<Edge>();

        }
        void elementaryEdges() {
            switch (elementaryOpertaion)
            {
            case ElementaryOperation.NOT:
                listInput.Add(new Edge(null, false, this) );
                break;
            case ElementaryOperation.NOT_AND:
                listOutput[0].push(!(listInput[0].signal && listInput[1].signal));
                break;
            case ElementaryOperation.NOT_OR:
                listOutput[0].push(!(listInput[0].signal || listInput[1].signal));
                break; 
            }
        }
        

        bool processElement()
        {
            if (isElementary)
            {
                switch (elementaryOpertaion)
                {
                    case ElementaryOperation.NOT:
                        listOutput[0].push(!listInput[0].signal);
                        break;
                    case ElementaryOperation.NOT_AND:
                        listOutput[0].push(!(listInput[0].signal && listInput[1].signal) );
                        break;
                    case ElementaryOperation.NOT_OR:
                        listOutput[0].push(!(listInput[0].signal || listInput[1].signal) );
                        break;
                }
            }
            foreach (var input in listInput) {
                if (input.to.isElementary)
                {
                    input.to.processElement();
                }
            }
            return true;
        }
        public static void processAll() {
            var temp = queue;
            queue = nextQueue;
            nextQueue = temp;
            foreach (var edge in queue)
            {

            }
        }
    }
}
