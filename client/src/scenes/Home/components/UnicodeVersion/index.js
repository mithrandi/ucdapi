import React from 'react'
import Panel from 'react-bootstrap/lib/Panel'
import Form from 'react-bootstrap/lib/Form'
import FormControl from 'react-bootstrap/lib/FormControl'
import ControlLabel from 'react-bootstrap/lib/ControlLabel'
import {setPropTypes} from 'recompose'
import T from 'prop-types'
import IT from 'react-immutable-proptypes'


const Versions = setPropTypes({
    version: T.string,
    versions: IT.listOf(T.string),
})(({version, versions}) => (
    <FormControl componentClass="select" placeholder="Unicode version" value={version}>
      {versions.map(v => <option value={v}>{v}</option>)}
    </FormControl>
))


const UnicodeVersion = setPropTypes({
    version: T.string,
    versions: IT.listOf(T.string),
})(({version, versions}) => (
    <Panel>
      <Form inline>
        <ControlLabel>Unicode version</ControlLabel>
        {' '}
        {versions ?
         <Versions version={version} versions={versions} /> :
         <FormControl.Static>…</FormControl.Static>}
      </Form>
    </Panel>
))

export default UnicodeVersion
