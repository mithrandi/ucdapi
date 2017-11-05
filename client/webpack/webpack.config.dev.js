const {resolve} = require('path')
const webpack = require('webpack')
const HtmlWebpackPlugin = require('html-webpack-plugin')

module.exports = {
    entry: [
        'webpack-dev-server/client',
        'webpack/hot/only-dev-server',
        resolve(__dirname, 'hotReload'),
    ],
    output: {
        filename: 'bundle.js',
        path: resolve(__dirname),
        publicPath: '/',
    },
    context: resolve(__dirname, '../src'),
    devtool: 'cheap-module-source-map',
    devServer: {
        hot: true,
        host: '127.0.0.1',
        contentBase: resolve(__dirname, '../assets'),
        publicPath: '/',
        historyApiFallback: true,
        proxy: {
            '/unicode': {
                target: 'http://127.0.0.1:3000/',
            },
        },
    },
    module: {
        rules: [
            {
                test: /\.(js|jsx)$/,
                include: [resolve(__dirname, '../src'), resolve(__dirname)],
                use: 'babel-loader',
            },
            {
                test: /\.s?css$/,
                use: [
                    {
                        loader: 'style-loader',
                    },
                    {
                        loader: 'css-loader',
                        options: {
                            importLoaders: 1,
                            sourceMap: true,
                        },
                    },
                    {
                        loader: 'resolve-url-loader',
                    },
                ],
            },
            {
                test: /\.(jpe?g|png|gif|svg|woff2?|eot|ttf)$/,
                use: [
                    {
                        loader: 'file-loader',
                        options: {
                            name: 'dist/[name].[hash].[ext]',
                        },
                    },
                ],
            },
        ],
    },
    resolve: {
        alias: {
            root: resolve(__dirname, '..', 'src'),
            npm: resolve(__dirname, '..', 'node_modules'),
        },
    },
    plugins: [
        new webpack.HotModuleReplacementPlugin(),
        new webpack.NamedModulesPlugin(),
        new HtmlWebpackPlugin({
            title: 'ucdapi.org',
            template: '../webpack/template.html',
        }),
    ],
    performance: { hints: false },
}
