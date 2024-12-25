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

        private Logger _b;
        public string myText { get; set; }
        public MainWindow()
        {
            InitializeComponent();
            _b  = new Logger(5);
            myText = "";

        }



        private void Calculate_Expression(object sender, RoutedEventArgs e)
        {
            string expressionText = ExpressionInput.Text;
            //bool noLetters =  !Regex.IsMatch(userText, @"[a-zA-Z]");

            //ErrorText.Text = "";
            //ExpressionResult.Text = "";
            //WarningText.Text = "";

            if (expressionText.Trim().Length <= 0)
            {
                ExpressionOutput.Text = "Empty expression..!";
                ExpressionInput.Text = "";
                return;
            }

            //LastLog.Text = "";
            SharedTypes.ExpressionResult interpreterOutput = Interpreter.Interpreter.evaluate(expressionText);
            //Debug.WriteLine("%d", output);
            //_b.setLogs($"{expressionText}: {output}");

            string output = interpreterOutput.ToString();
            string userOutput = "";

            if (output.Contains('"'))
            {
                //string[] outs  = output.ToString().Split("\"");
                //Debug.WriteLine(outs[0]);
                //Debug.WriteLine(outs[1]);
                userOutput = Check_Output(output);
            } else
            {
                userOutput = output;
            }

            Debug.Write("user output: ");
            Debug.WriteLine(userOutput);
            ExpressionOutput.Text = output;  // send either direct or parsed output - it is direct as of now

            _b.setLogs($"{expressionText}: {output}");

            string[] ss = _b.getLogs();
            Debug.WriteLine("yayy",_b.getLogs());

            TextBlock textBlock = (TextBlock)FindName("Logsss");
            textBlock.Text = "";

            for (int i = 0; i < ss.Length; i++)
            {
                Debug.Write("logs");
                Debug.WriteLine(ss[i]);
                textBlock.Text += "\n"+ss[i];
                
            }

            Debug.Write("mytext");
            Debug.WriteLine(myText);

        }

        private void TextBox_TextChanged(object sender, TextChangedEventArgs e)
        {
            //string userText = UserInputText.Text;
            // evaluate expression on enter

        }

        private void ExpressionInput_PreviewKeyDown(object sender, KeyEventArgs e)
        {
            if (e.Key == Key.Enter)
            {
                    Calculate_Expression(sender, e);
            }
        }

        private void ListBox_SelectionChanged(object sender, SelectionChangedEventArgs e)
        {

        }

        private void TextBox_TextChanged_1(object sender, TextChangedEventArgs e)
        {

        }

        private void Plot_Polynomial(object sender, RoutedEventArgs e) {

            string polynomialText = PolynomialInput.Text;
            ExpressionOutput.Text = "";
            //string userText2 = rangeText.Text;

            //Debug.WriteLine(userText2);

            bool hasALog = polynomialText.Contains("log");

            string[] rangeText = PolynomialRange.Text.Split(',');
            double initialValue = double.Parse(rangeText[0]);
            double finalValue = double.Parse(rangeText[1]);

            double stepInput = double.Parse(PolynomialStep.Text.ToString());

            //int ttt = Interpreter.main([userText1]);


            List<double> xList = new List<double> { };
            List<double> yList = new List<double> { };

            if (polynomialText.Trim().Length == 0)
            {
                ExpressionOutput.Text = "Empty polynomial expression!";
                PolynomialInput.Text = "";
                return;
            }

            if (hasALog && initialValue <= 0)
            {
                //MessageBox.Show("log can be calculated for only positive integers");
                ExpressionOutput.Text = "Log can only be plotted for positive integer values, update plotting range.";
                return;
            }

            for (double i = initialValue; i < finalValue; i = i + stepInput)
            {
                SharedTypes.ExpressionResult tr = Interpreter.Interpreter.evaluate($"var x={i}");
                Debug.Write("asda ss ");
                Debug.WriteLine(tr);
                Debug.WriteLine($"x={i}");
                Debug.WriteLine(polynomialText);
                SharedTypes.ExpressionResult plotY = Interpreter.Interpreter.evaluate(polynomialText);

                //string[] returnOutput = Output.result.Output.Split(' ');

                Debug.Write("ttt---");
                Debug.WriteLine(plotY);
                xList.Add(i);
                yList.Add(double.Parse(Check_Output(plotY.ToString())));
            }

            //int tt = Interpreter.main([userText1]);

            double[] xArr = xList.ToArray();
            double[] yArr = yList.ToArray();


            var graph = WpfPlot1.Plot.Add.Scatter(xArr, yArr);
            graph.MarkerSize = 0;
            graph.LineWidth = 2;
            WpfPlot1.Refresh();
            int count = 1;
            plot_graph(xArr, yArr, polynomialText);
            count++;

        }

        private void Button_Click1(object sender, RoutedEventArgs e)
        {
            
            //WpfPlot1.Plot.Add.Scatter(array11, array22);
            //WpfPlot1.Refresh();
            //WpfPlot1.Focus();
            //WpfPlot1.UpdateLayout();

            //WpfPlot1.PlotStyle.DotStyle
        }

        private void plot_graph(double[] xData, double[] yData, string plotName)
        {
            var graph = WpfPlot1.Plot.Add.Scatter(xData, yData);
            graph.MarkerSize = 0;
            graph.LineWidth = 2;
            graph.LegendText = plotName;
            WpfPlot1.Refresh();
        }

        private string Check_Output(string output)
        {
            string[] outArr = output.ToString().Split("\"");
            Debug.WriteLine(outArr[0]);
            Debug.WriteLine(outArr[1]);
            //userOutput = outs[1];
            int lastIndex = outArr.Length - 1;
            for(int  i = 0; i < outArr.Length; i++)
            {
                Debug.Write("arr");
                Debug.WriteLine(outArr[i]);
            }
            return outArr[outArr.Length - 2];
        }

        
    }

    public class Logger
    {
        //public string[] logs;
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
                logs = [];
            }

            logs.Add(str);
            Debug.Write("set logs");
            Debug.WriteLine(logs.Count);
        }

    }
}

// ADDITIONS TO UI:
//1. add history of the commands and results into the UI for interpreter
