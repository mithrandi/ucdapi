const { resolve } = require('path')
const webpack = require('webpack')
const HtmlWebpackPlugin = require('html-webpack-plugin')
const PreloadWebpackPlugin = require('preload-webpack-plugin')
const SplitByPathPlugin = require('webpack-split-by-path')

module.exports = {
    entry: {
        main: resolve(__dirname, '../src'),
    },
    output: {
        filename: '[name].[chunkhash].js',
        path: resolve(__dirname, '../dist'),
        publicPath: '/',
    },
    module: {
        rules: [
            {
                test: /\.(js|jsx)$/,
                include: [resolve(__dirname, '../src')],
                use: 'babel-loader',
            },
        ],
    },
    resolve: {
        alias: {
            root: resolve(__dirname),
            npm: resolve(__dirname, '..', 'node_modules'),
        },
    },
    plugins: [
        new webpack.optimize.ModuleConcatenationPlugin(),
        new webpack.DefinePlugin({
            'process.env': {
                NODE_ENV: JSON.stringify('production'),
            },
        }),
        new SplitByPathPlugin([
            {
                name: 'vendor',
                path: resolve(__dirname, '..', 'node_modules')
            }
        ]),
        new webpack.optimize.UglifyJsPlugin(),
        new HtmlWebpackPlugin({
            filename: 'index.html',
            title: 'ucdapi.org',
            template: 'webpack/template.html',
        }),
        new PreloadWebpackPlugin({
            rel: 'preload',
            as: 'script',
            include: 'all',
        }),
    ],
}
