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
    versionChanged: T.func.isRequired,
})(({version, versions, versionChanged}) => (
    <FormControl componentClass="select"
                 placeholder="Unicode version"
                 value={version}
                 onChange={e => versionChanged(e.target.value)}>
      {versions.map(v => <option value={v} key={v}>{v}</option>)}
    </FormControl>
))


const UnicodeVersion = setPropTypes({
    version: T.string,
    versions: IT.listOf(T.string),
    versionChanged: T.func.isRequired,
})(({version, versions, versionChanged}) => (
    <Panel>
      <Form inline>
        <ControlLabel>Unicode version</ControlLabel>
        {' '}
        {versions ?
         <Versions version={version}
                   versions={versions}
                   versionChanged={versionChanged} /> :
         <FormControl.Static>…</FormControl.Static>}
      </Form>
    </Panel>
))

export default UnicodeVersion
