var path = require("path");
var CopyWebpackPlugin = require('copy-webpack-plugin');

module.exports = {
  entry: {
    app: [
      './src/index.js'
    ]
  },

  output: {
    path: path.resolve(__dirname + '/dist'),
    filename: '[name].js',
  },

  plugins: [
       new CopyWebpackPlugin([
           { from: './js/asciidoctor.js', to: 'js/asciidoctor.js'},
           { from: './js/main.js', to: 'js/main.js'},
           { from: './js/MathJax.js', to: 'js/MathJax.js'},
           { from: './src/css/asciidoc2.css', to: 'src/css/asciidoc2.css'},
           { from: './src/css/extra.css', to: 'src/css/extra.css'}

        ])
    ],

  module: {
    rules: [
      {
        test: /\.(css|scss)$/,
        use: [
          'style-loader',
          'css-loader',
        ]
      },
      {
        test:    /\.html$/,
        exclude: /node_modules/,
        loader:  'file-loader?name=[name].[ext]',
      },
      {
        test:    /\.elm$/,
        exclude: [/elm-stuff/, /node_modules/],
        loader: 'elm-webpack-loader',
        options: {
          verbose: true,
          debug: false,
          warn: true,
          pathToMake: '.bin/unbuffered-elm-make',

        },
      },
      {
        test: /\.woff(2)?(\?v=[0-9]\.[0-9]\.[0-9])?$/,
        loader: 'url-loader?limit=10000&mimetype=application/font-woff',
      },
      {
        test: /\.(ttf|eot|svg)(\?v=[0-9]\.[0-9]\.[0-9])?$/,
        loader: 'file-loader',
      },
    ],

    noParse: /\.elm$/,
  },

  devServer: {
    inline: true,
    stats: { colors: true },
  },


};
