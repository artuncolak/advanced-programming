using System.Text;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Data;
using System.Windows.Documents;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using System.Windows.Navigation;
using System.Windows.Shapes;
using System.Linq;
using System.Text.RegularExpressions;
using Interpreter;
using static Microsoft.FSharp.Core.ByRefKinds;
using System.Diagnostics;
using System.Collections.Generic;

namespace WpfGui
{
    /// <summary>
    /// Interaction logic for MainWindow.xaml
    /// </summary>
    /// 
    public partial class MainWindow : Window
    {

        private Logger _LogIns;
        public string LogText { get; set; }

        private List<ScottPlot.Plottables.Scatter> Graph;
        public MainWindow()
        {
            InitializeComponent();
            _LogIns  = new Logger(5);
            LogText = "";
            Graph = new List<ScottPlot.Plottables.Scatter> { };

        }



        private void Calculate_Expression(object sender, RoutedEventArgs e)
        {
            string expressionText = ExpressionInput.Text;
            //bool noLetters =  !Regex.IsMatch(userText, @"[a-zA-Z]");


            if (expressionText.Trim().Length <= 0)
            {
                ExpressionOutput.Text = "Empty expression..!";
                ExpressionInput.Text = "";
                return;
            }

            SharedTypes.ExpressionResult interpreterOutput = Interpreter.Interpreter.evaluate(expressionText);
          

            string output = interpreterOutput.ToString();
            string userOutput = "";

            if (output.Contains('"'))
            {
                userOutput = Check_Output(output);
            } else
            {
                userOutput = output;
            }

            Debug.Write("user output: ");
            Debug.WriteLine(userOutput);
            ExpressionOutput.Text = userOutput; 

            _LogIns.setLogs($"{expressionText} :- {userOutput}");

            string[] logArr = _LogIns.getLogs();
            Debug.WriteLine("yayy",_LogIns.getLogs());

            TextBlock textBlock = (TextBlock)FindName("Logs");
            textBlock.Text = "";

            for (int i = 0; i < logArr.Length; i++)
            {
                Debug.Write("logs");
                Debug.WriteLine(logArr[i]);
                textBlock.Text += "\n"+logArr[i];
                
            }

            Debug.Write("mytext");
            Debug.WriteLine(LogText);

        }
     
        private void ExpressionInput_PreviewKeyDown(object sender, KeyEventArgs e)
        {
            if (e.Key == Key.Enter)
            {
                    Calculate_Expression(sender, e);
            }
        }

        private void Plot_Polynomial(object sender, RoutedEventArgs e) {

            string polynomialText = PolynomialInput.Text;
            ExpressionOutput.Text = "";

            if (polynomialText.Trim().Length == 0)
            {
                ExpressionOutput.Text = "Empty polynomial expression!";
                PolynomialInput.Text = "";
                return;
            }

            bool hasALog = polynomialText.Contains("log");

            if (PolynomialRange.Text.Trim().Length == 0)
            {
                ExpressionOutput.Text = "Empty polynomial range!";
                PolynomialInput.Text = "0,10";
                return;
            }

            bool hasCommaAsSeparator = PolynomialRange.Text.Contains(",");
            bool noLettersRange =  Regex.IsMatch(PolynomialRange.Text.Trim(), @"^[-]?\d{1,}?.[-]?\d{1,},[-]?\d{1,}?.[-]?\d{1,}$|^[-]?\d{1,},[-]?\d{1,}$|^[-]?\d{1,}?.[-]?\d{1,},[-]?\d{1,}$|^[-]?\d{1,},[-]?\d{1,}.[-]?\d{1,}$");


            if (!noLettersRange)
            {
                ExpressionOutput.Text = "Invalid range format!";
                PolynomialRange.Text = "0,10";
                return;
            }

            string[] rangeText = PolynomialRange.Text.Trim().Split(',');

            Debug.Write("le");
            Debug.WriteLine(rangeText.Length);
            Debug.WriteLine(rangeText);


            if (rangeText.Length != 2)
            {
                ExpressionOutput.Text = "Invalid range format!";
                PolynomialRange.Text = "0,10";
                return;
            }

            double initialValue = double.Parse(rangeText[0]);
            double finalValue = double.Parse(rangeText[1]);

            if (initialValue > finalValue)
            {
                ExpressionOutput.Text = "Final value must be greater than initial value.";
                return;
            }

            if (initialValue - finalValue == 0)
            {
                ExpressionOutput.Text = "Initial, Final value must be not be same values.";
                return;
            }

            if (hasALog && initialValue <= 0)
            {
                ExpressionOutput.Text = "Log can only be plotted for positive integer values, update plotting range.";
                return;
            }

            if (PolynomialStep.Text.Trim().Length == 0)
            {
                ExpressionOutput.Text = "Empty polynomial step!";
                PolynomialStep.Text = "0.1";
                return;
            }

            bool noLetterStep =  Regex.IsMatch(PolynomialStep.Text.Trim(), @"^\d{1,}$|^\d{1,}?.\d{1,}$");

            if (!noLetterStep)
            {
                ExpressionOutput.Text = "Invalid value in step!";
                PolynomialStep.Text = "0.1";
                return;
            }

            double stepInput = double.Parse(PolynomialStep.Text.ToString());

            if (stepInput <= 0)
            {
                ExpressionOutput.Text = "Step should be greater than 0!";
                PolynomialStep.Text = "0.1";
                return;
            }

            List<double> xList = new List<double> { };
            List<double> yList = new List<double> { };

            for (double i = initialValue; i <= finalValue; i = i + stepInput)
            {
                SharedTypes.ExpressionResult plotX = Interpreter.Interpreter.evaluate($"var x={i}");
                Debug.Write("asda logArr ");
                Debug.WriteLine(plotX);
                Debug.WriteLine($"x={i}");
                Debug.WriteLine(polynomialText);
                SharedTypes.ExpressionResult plotY = Interpreter.Interpreter.evaluate(polynomialText);

                string plotYCheck = plotY.ToString();

                if(!plotYCheck.Contains("ValidOutput"))
                {
                    ExpressionOutput.Text = "Invalid format of polynomial";
                    return;
                }
               
                Debug.Write("ttt---");
                Debug.WriteLine(plotY);
                xList.Add(i);
                yList.Add(double.Parse(Check_Output(plotY.ToString())));
            }

            double[] xArr = xList.ToArray();
            double[] yArr = yList.ToArray();

            Plot_Graph(xArr, yArr, polynomialText);
           
        }

        private void Plot_Graph(double[] xData, double[] yData, string plotName)
        {

            var currentGraph = WpfPlot.Plot.Add.Scatter(xData, yData);
            currentGraph.MarkerSize = 0;
            currentGraph.LineWidth = 2;
            currentGraph.LegendText = plotName;
            Graph.Add(currentGraph);
            WpfPlot.Refresh();
        }

        private void Clear_Plot(object sender, RoutedEventArgs e)
        {
            if (Graph.Count != 0)
            {
                WpfPlot.Plot.Remove(Graph[0]);
                Graph.RemoveAt(0);

            }
            WpfPlot.Refresh();



        }
        private void Clear_All_Plots(object sender, RoutedEventArgs e)
        {
            
            if (Graph.Count > 0)
            {
                int count = Graph.Count;
                while(count > 0)
                {
                    WpfPlot.Plot.Remove(Graph[count-1]);
                    count--;
                }
               
                Graph.Clear();
                WpfPlot.Refresh();

            }

        }

        private void Toggle_Marker(object sender, RoutedEventArgs e)
        {
            if (Graph.Count != 0)
            {
                for (int i = 0; i < Graph.Count; i++)
                {
                    Graph[i].MarkerSize = Graph[i].MarkerSize == 5 ? 0 : 5;
                }
                WpfPlot.Refresh();

            }
        }


        private string Check_Output(string output)
        {
           
            Debug.Write("actual output");
            Debug.WriteLine(output);

            Regex regex = new Regex(@"""\""?([^""]*)\""?""");
            Match match  = regex.Match(output);
            string matchedContent = "";
            if (match.Success) {
                matchedContent = match.Value.Substring(1, match.Value.Length-2);
                
            } else
            {
                matchedContent = "0";
            }

            return matchedContent;

        }

    }

    public class Logger
    {
        
        public List<string> logs;

        public Logger(int size)
        {
            logs = new List<string> { };
        }

        public string[] getLogs()
        {
            Debug.Write("get logs");
            Debug.WriteLine(logs.Count);


            return logs.ToArray();
        }

        public void setLogs( string str)
        {
            if (logs.Count == 6)
            {
                logs.RemoveAt(0);
            }

            logs.Add(str);
            Debug.Write("set logs");
            Debug.WriteLine(logs.Count);
        }

    }
}

