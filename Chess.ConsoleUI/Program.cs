using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using static Chess.Domain.Api;

namespace Chess.ConsoleUI
{
    class Program
    {
        static void Main(string[] args)
        {
            var api = new ChessApi();
            RenderGame(api);
        }

        static void RenderGame(ChessApi api)
        {
            /* The board:
             
                    A B C D E F G H
                    ─ ─ ─ ─ ─ ─ ─ ─
                8 | r n b q k b n r
                7 | p p p p p p p p
                6 | . . . . . . . .
                5 | . . . . . . . .
                4 | . . . . . . . .
                3 | . . . . . . . .
                2 | P P P P P P P P
                1 | R N B K Q B N R
            */

            Console.WriteLine();
            Console.WriteLine("    A B C D E F G H");
            Console.WriteLine("    ─ ─ ─ ─ ─ ─ ─ ─");

            var rows = new[] { "8", "7", "6", "5", "4", "3", "2", "1" };
            var cols = new[] { "A", "B", "C", "D", "E", "F", "G", "H" };
            var cells = api.Cells.ToDictionary(c => c.coord);

            foreach (var row in rows)
            {
                Console.Write("{0} |", row);

                foreach (var col in cols)
                {
                    var cell = cells[col + row];
                    Console.Write(" {0}", RenderCell(cell));
                }

                Console.WriteLine();
            }

            Console.WriteLine();
            Console.WriteLine(api.Message);

            // TODO: Add game over state to the public facing api and handle here

            Console.WriteLine("Enter a move in the following format: A2 A3 (case insensitive)");
            Console.Write("> ");

            var moveText = Console.ReadLine();
            var move = new ParsedMove(moveText);
            if (move.IsValid)
            {
                try
                {
                    api.Move(move.From, move.To);
                }
                catch (Exception ex)
                {
                }
            }

            Console.Clear();
            RenderGame(api);
        }

        static string RenderCell(CellDTO cell)
        {
            switch (cell)
            {
                case CellDTO c when !c.isOccupied:
                    return ".";

                case CellDTO c when c.color == "black" && c.rank == "pawn":
                    return "p";
                case CellDTO c when c.color == "black" && c.rank == "rook":
                    return "r";
                case CellDTO c when c.color == "black" && c.rank == "knight":
                    return "n";
                case CellDTO c when c.color == "black" && c.rank == "bishop":
                    return "b";
                case CellDTO c when c.color == "black" && c.rank == "queen":
                    return "q";
                case CellDTO c when c.color == "black" && c.rank == "king":
                    return "k";

                case CellDTO c when c.color == "white" && c.rank == "pawn":
                    return "P";
                case CellDTO c when c.color == "white" && c.rank == "rook":
                    return "R";
                case CellDTO c when c.color == "white" && c.rank == "knight":
                    return "N";
                case CellDTO c when c.color == "white" && c.rank == "bishop":
                    return "B";
                case CellDTO c when c.color == "white" && c.rank == "queen":
                    return "Q";
                case CellDTO c when c.color == "white" && c.rank == "king":
                    return "K";

                default:
                    throw new Exception("Invalid piece.");
            }
        }

        class ParsedMove
        {
            public ParsedMove(string move)
            {
                var parts = (move ?? string.Empty).ToUpper().Split(new char[] { ',', ' ', '-' });
                if (parts.Length == 2)
                {
                    IsValid = true;
                    From = parts[0];
                    To = parts[1];
                }
                else
                {
                    IsValid = false;
                }
            }

            public string From { get; private set; }
            public string To { get; private set; }
            public bool IsValid { get; private set; }
        }
    }
}
