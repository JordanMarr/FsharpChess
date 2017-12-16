using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Data;
using System.Windows.Documents;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using System.Windows.Navigation;
using System.Windows.Shapes;
using Chess.Domain;

namespace Chess.WpfUI
{
    /// <summary>
    /// Interaction logic for MainWindow.xaml
    /// </summary>
    public partial class MainWindow : Window
    {
        Domain.Api.ChessApi _api;
        List<QueuedCell> _moveQueue;


        public MainWindow()
        {
            InitializeComponent();

            _api = new Domain.Api.ChessApi();
            _moveQueue = new List<QueuedCell>();
        }

        private void Window_Loaded(object sender, RoutedEventArgs e)
        {
            RefreshBoard();
        }

        private void RefreshBoard()
        {
            foreach (var cell in _api.GameState.cells)
            {
                var border = (Border)FindName(cell.coord);
                var button = border.Child as Button;
                var img = button.Content as Image;
                if (cell.isOccupied)
                {
                    var path = $"/Images/pieces_{cell.color}/{cell.rank}.png";
                    var bm = new BitmapImage(new Uri(path, UriKind.Relative));
                    img.Source = bm;
                }
                else
                {
                    img.Source = null;
                }
            }

            StatusMessage.Text = _api.GameState.message;
        }

        private void Cell_Click(object sender, RoutedEventArgs e)
        {
            var btn = sender as Button;
            var border = btn.Parent as Border;

            if (_moveQueue.FirstOrDefault()?.Coord == border.Name)
            {
                _moveQueue.ForEach(q => q.RevertColor());
                _moveQueue.Clear();
                return; // Can't click a cell twice
            }

            _moveQueue.Add(new QueuedCell(border));
            
            if (_moveQueue.Count > 1)
            {
                _api.Move(_moveQueue[0].Coord, _moveQueue[1].Coord);
                _moveQueue.ForEach(q => q.RevertColor());
                _moveQueue.Clear();
                RefreshBoard();
            }
        }


        private class QueuedCell
        {
            public QueuedCell(Border border)
            {
                Border = border;
                Coord = border.Name;
                OrigColor = (border.Background as SolidColorBrush).Color;

                // Highlight cell
                border.Background = new SolidColorBrush(Colors.LightPink);
            }

            public Border Border { get; private set; }
            public string Coord { get; private set; }
            public Color OrigColor { get; private set; }

            public void RevertColor()
            {
                Border.Background = new SolidColorBrush(OrigColor);
            }
        }
    }
}
