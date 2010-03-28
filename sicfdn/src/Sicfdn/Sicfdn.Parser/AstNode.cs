using System;
using System.Collections.Generic;
using System.Text;

namespace Sicfdn.Parser
{
    public class AstNode
    {
        private AstNodeType type;
        public AstNodeType Type
        {
            get { return type; }
        }

        private Token token;
        public Token Token
        {
            get { return token; }
        }

        private List<AstNode> children;
        public List<AstNode> Children
        {
            get { return children; }
        }

        //@begin(ctor)
        public AstNode(
            AstNodeType type, IEnumerable<AstNode> children)
        {
            this.type = type;
            this.token = null;
            this.children = new List<AstNode>();
            this.children.AddRange(children);
        }

        public AstNode(
            AstNodeType type, Token token)
        {
            this.type = type;
            this.token = token;
            this.children = null;
        }
        //@end(ctor)

        public override string ToString()
        {
            return this.ToString(0);
        }

        public string ToString(int indent)
        {
            StringBuilder sb = new StringBuilder();
            sb.AppendLine(String.Format(
                "{0}{1}: {2}", 
                new String(' ', indent), 
                this.Type, 
                this.Token != null ? this.Token.Content : ""));
            if (this.Children != null)
            {
                foreach (AstNode subNode in this.Children)
                {
                    sb.Append(subNode.ToString(indent + 4));
                }
            }
            return sb.ToString();
        }
    }
}
